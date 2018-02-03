Alternative Quick Start using cabal
-----------------------------------

In case you have problems with `stack`, here is a more old-fashioned
way to work with the exercises.

1. Install [The Haskell Platform](https://www.haskell.org/platform/)

2. Download dpendencies and check that you can run the tests:

        $ cabal sandbox init
        $ cabal exec runhaskell W0Test.hs

    This should print `Everything seems to be OK!`. If you see any errors,
    you might not have a problem with your Haskell installation.

4. Generate the exercise templates (files `W*.hs`):

        $ make

5. Now you can edit `W1.hs` and see how well you did by running

        $ cabal exec runhaskell W1Test.hs
