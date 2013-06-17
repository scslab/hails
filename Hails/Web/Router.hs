{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{- |

Conceptually, a route is function that, given an HTTP request, may return
an action (something that would return a response for the client if run).
Routes can be concatenated--where each route is evaluated until one
matches--and nested. Routes are expressed through the 'Routeable' type class.
'runRoute' transforms an instance of 'Routeable' to a function from 'Request'
to a monadic action (in the 'ResourceT' monad) that returns a
'Maybe' 'Response'. The return type was chosen to be monadic so routing
decisions can depend on side-effects (e.g. a random number or counter for A/B
testing, IP geolocation lookup etc').

-}

module Hails.Web.Router
  (
  -- * Example
  -- $Example
    Routeable(..)
  , mkRouter
  -- * Route Monad
  , Route, RouteM(..)
  -- * Common Routes
  , routeAll, routeHost, routeTop, routeMethod
  , routePattern, routeName, routeVar
  ) where

import           Prelude hiding (pi)

import           LIO
import           LIO.DCLabel

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Hails.HttpServer
import           Hails.Web.Responses

-- | Route handler is a fucntion from the path info, request
-- configuration, and labeled request to a response.
type RouteHandler =  [Text]             -- ^ Path info
                  -> [(S8.ByteString, Maybe S8.ByteString)]        -- ^ Extra query params
                  -> RequestConfig      -- ^ Request configuration
                  -> DCLabeled Request  -- ^ Labeled request
                  -> DC (Maybe Response)

{- |
'Routeable' types can be converted into a route function using 'runRoute'.
If the route is matched it returns a 'Response', otherwise 'Nothing'.

In general, 'Routeable's are data-dependant (on the 'Request'), but don't have
to be. For example, 'Application' is an instance of 'Routeable' that always
returns a 'Response':

@
  instance Routeable Application where
    runRoute app req = app req >>= return . Just
@

-}
class Routeable r where
  -- | Run a route
  runRoute :: r -> RouteHandler

-- | Converts any 'Routeable' into an 'Application' that can be passed
-- directly to a WAI server.
mkRouter :: Routeable r => r -> Application
mkRouter route conf lreq = do
  req <- liftLIO $ unlabel lreq
  let pi = pathInfo req
  mapp <- runRoute route pi [] conf lreq
  case mapp of
    Just resp -> return resp
    Nothing -> return notFound


instance Routeable Application where
  runRoute app _ _ conf req = fmap Just $ app conf req

instance Routeable Response where
  runRoute resp _ _ _  _ = return . Just $ resp

{- |
The 'RouteM' type is a basic instance of 'Routeable' that simply holds
the routing function and an arbitrary additional data parameter. In
most cases this paramter is simply '()', hence we have a synonym for
@'RouteM' '()'@ called 'Route'.  The power is derived from the
instances of 'Monad' and 'Monoid', which allow the simple construction
of complex routing rules using either lists ('Monoid') or do-notation.
Moreover, because of it's simple type, any 'Routeable' can be used as
a 'Route' (using 'routeAll' or by applying it to 'runRoute'), making
it possible to leverage the monadic or monoid syntax for any
'Routeable'.

Commonly, route functions that construct a 'Route' only inspect the 'Request'
and other parameters. For example, 'routeHost' looks at the hostname:

@
  routeHost :: Routeable r => S.ByteString -> r -> Route
  routeHost host route = Route func ()
    where func req = if host == serverName req
                       then runRoute route req
                       else return Nothing
@

However, because the result of a route is in the
'ResourceT' monad, routes have all the power of an 'Application' and can make
state-dependant decisions. For example, it is trivial to implement a route that
succeeds for every other request (perhaps for A/B testing):

@
  routeEveryOther :: (Routeable r1, Routeable r2)
                  => MVar Int -> r1 -> r2 -> Route
  routeEveryOther counter r1 r2 = Route func ()
    where func req = do
            i <- liftIO . modifyMVar $ \i ->
                    let i' = i+1
                    in return (i', i')
            if i `mod` 2 == 0
              then runRoute r1 req
              else runRoute r2 req
@

-}
data RouteM a = Route RouteHandler a

-- | Synonym for 'RouteM', the common case where the data parameter is '()'.
type Route = RouteM ()

-- | Create a route given the route handler.
mroute :: RouteHandler -> Route
mroute handler = Route handler ()

instance Monad RouteM where
  return a = Route (const . const . const . const $ return Nothing) a
  (Route rtA valA) >>= fn =
    let (Route rtB valB) = fn valA
    in Route (\pi eq conf req -> do
      resA <- rtA pi eq conf req
      case resA of
        Nothing -> rtB pi eq conf req
        Just _ -> return resA) valB

instance Monoid Route where
  mempty = mroute $ const . const . const . const $ return Nothing
  mappend (Route a _) (Route b _) = mroute $ \pi eq conf req -> do
    c <- a pi eq conf req
    case c of
      Nothing -> b pi eq conf req
      Just _ -> return c

instance Routeable (RouteM a) where
  runRoute (Route rtr _) pi eq conf req = rtr pi eq conf req

-- | A route that always matches (useful for converting a 'Routeable' into a
-- 'Route').
routeAll :: Routeable r => r -> Route
routeAll = mroute . runRoute

-- | Matches on the hostname from the 'Request'. The route only successeds on
-- exact matches.
routeHost :: Routeable r => S.ByteString -> r -> Route
routeHost host route = mroute $ \pi eq conf lreq -> do
  req <- unlabel lreq
  if host == serverName req
    then runRoute route pi eq conf lreq
    else return Nothing

-- | Matches if the path is empty. Note that this route checks that 'pathInfo'
-- is empty, so it works as expected when nested under namespaces or other
-- routes that pop the 'pathInfo' list.
routeTop :: Routeable r => r -> Route
routeTop route = mroute $ \pi eq conf lreq -> do
  if null pi || (T.null . head $ pi)
    then runRoute route pi eq conf lreq
    else return Nothing

-- | Matches on the HTTP request method (e.g. 'GET', 'POST', 'PUT')
routeMethod :: Routeable r => StdMethod -> r -> Route
routeMethod method route = mroute $ \pi eq conf lreq -> do
  req <- unlabel lreq
  if renderStdMethod method == requestMethod req then
    runRoute route pi eq conf lreq
    else return Nothing

-- | Routes the given URL pattern. Patterns can include
-- directories as well as variable patterns (prefixed with @:@) to be added
-- to 'queryString' (see 'routeVar')
--
--  * \/posts\/:id
--
--  * \/posts\/:id\/new
--
--  * \/:date\/posts\/:category\/new
--
routePattern :: Routeable r => S.ByteString -> r -> Route
routePattern pattern route =
  let patternParts = map T.unpack $ decodePathSegments pattern
  in foldr mkRoute (routeTop route) patternParts
  where mkRoute (':':varName) = routeVar (S8.pack varName)
        mkRoute varName = routeName (S8.pack varName)

-- | Matches if the first directory in the path matches the given 'ByteString'
routeName :: Routeable r => S.ByteString -> r -> Route
routeName name route = mroute $ \pi eq conf lreq -> do
  if (not . null $ pi) && S8.unpack name == (T.unpack . head $ pi)
    then runRoute route (tail pi) eq conf lreq
    else return Nothing

-- | Always matches if there is at least one directory in 'pathInfo' but and
-- adds a parameter to 'queryString' where the key is the supplied
-- variable name and the value is the directory consumed from the path.
routeVar :: Routeable r => S.ByteString -> r -> Route
routeVar varName route = mroute $ \pi eq conf lreq ->
  if null pi
    then return Nothing
    else let varVal = S8.pack . T.unpack . head $ pi
             neqp = (varName, Just varVal):eq
         in runRoute route (tail pi) neqp conf lreq

{- $Example
 #example#

The most basic 'Routeable' types are 'Application' and 'Response'. Reaching
either of these types marks a termination in the routing lookup. This module
exposes a monadic type 'Route' which makes it easy to create routing logic
in a DSL-like fashion.

'Route's are concatenated using the '>>' operator (or using do-notation).
In the end, any 'Routeable', including a 'Route' is converted to an
'Application' and passed to the server using 'mkRouter':

@

  mainAction :: Application
  mainAction req = ...

  signinForm :: Application
  signinForm req = ...

  login :: Application
  login req = ...

  updateProfile :: Application
  updateProfile req = ...

  main :: IO ()
  main = runSettings defaultSettings $ mkRouter $ do
    routeTop mainAction
    routeName \"sessions\" $ do
      routeMethod GET signinForm
      routeMethod POST login
    routeMethod PUT $ routePattern \"users/:id\" updateProfile
    routeAll $ responseLBS status404 [] \"Are you in the right place?\"
@

-}

