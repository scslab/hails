module Main (main) where

import Test.Framework

import qualified AuthTests
import qualified HsonTests
import qualified DatabaseTests

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = AuthTests.tests
     ++ HsonTests.tests
     ++ DatabaseTests.tests
