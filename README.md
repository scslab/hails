# HAILS: Protecting Data Privacy in Untrusted Web Apps #

Hails is a platform and web framework that leverages Information Flow Control (IFC) to support untrusted and mutually distrustful web applications interacting and processing private data.

## Resources ##

 - Source: [GitHub](https://github.com/alevy/hails) (git clone git://github.com/alevy/hails.git)

## Requirements ##

### Haskell Packages ###

Hails requires a version of the `lio` package that's more up to date than on Hackage. To install the latest version
from the Git repository:

    $ git clone http://www.scs.stanford.edu/~deian/lio.git
    $ cd lio
    $ cabal configure
    (resolve any cabal dependancies)
    $ cabal install

## Compiling ##

You can compile Hails as usual with `cabal`:

    $ cabal configure
    $ cabal build

