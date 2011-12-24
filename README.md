# HAILS: Protecting Data Privacy in Untrusted Web Apps #

Hails is a platform and web framework that leverages Information Flow Control (IFC) to support untrusted and mutually distrustful web applications interacting and processing private data.

## Resources ##

 - Wiki: [Private](https://hails.scs.stanford.edu/)
 - Source: [GitHub](https://github.com/alevy/hails) (git clone git://github.com/alevy/hails.git)

## Requirements ##

### Haskell Packages ###

Hails requires a version of the `iterio-server` package that's more up to date than on Hackage. To install the latest version
from the Git repository:

    $ git clone http://github.com/alevy/iterio-server.git
    $ cd iterio-server
    $ cabal configure
    (resolve any cabal dependancies)
    $ cabal install

## Compiling ##

You can compile Hails as usual with `cabal`:

    $ cabal build

