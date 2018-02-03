Opqdonut's Haskell Exercises
============================

Quick Start
-----------

If these instructions don't work for you, see the
[instructions for using cabal instead of stack](README.cabal.md)

1. Install [Stack](https://www.haskellstack.org/)

2. Download dependencies by running

        $ stack build

2. Then check you can actually run the tests with:

        $ stack runhaskell W0Test.hs

    This should print `Everything seems to be OK!`. If you see any errors,
    you might not have a problem with your Haskell installation.

3. Generate the exercise templates (files `W*.hs`):

        $ make

4. Now you can edit `W1.hs` and see how well you did by running

        $ stack runhaskell W1Test.hs

Introduction
------------

This is a collection of small and simple Haskell exercises with unit
tests. This means your answers to the exercises are checked by the
computer – making these exercises great for self-study.

The exercises don't really include any reading material, so you should
study some Haskell tutorial, e.g. [Learn You A Haskell For Great
Good!]( http://learnyouahaskell.com/) while working on the exercises.

If bump into an exercise that talks about terms you don't understand,
look them up in a tutorial or google them! The exercises are meant to
encourage you to learn, not check that you already know stuff :)

I created this set of exercises for a Haskell course I held at
Helsinki University because I couldn't find a set of good Haskell
exercises anywhere. I hope somebody else finds them useful as well.

Contents
--------

The exercises are split into sets called "weeks", each containing
about 20 exercises. You are meant to work through the weeks in order,
but you don't need to get every exercise in a week right to move on.

The topics of the weeks are

* W1: basics, syntax, defining functions, pattern matching, recursion
* W2: lists, strings, higher-order functions, polymorphism
* W3: data types
* W4: IO
* W5: type classes
* W6: Monads
* W7: recap of weeks 1-6

Working on the Excercises
-------------------------

- Read and edit the `Wn.hs` files according to the instructions
- Don't remove or change any type signatures (things like `foo ::
  String -> String`) that are already in the files
- Check your answers for week `n` with `stack runhaskell WnTest.hs`
  (or alternatively `cabal exec runhaskell WnTest.hs` if you're not using stack)
- A typical test failure looks like this:

        Testing 11
        *** Failed! Falsifiable (after 1 test):
        0
        0
        Expected 1, got 3
        FAIL

    This means that the function from exercise 11 failed the test when
    the two arguments were 0 and 0. The result should have been 1, but
    it was 3.

    I'm sorry if the test failures aren't always understandable :/

- You can also play around with your solutions interactively by
  running `stack exec ghci Wn.hs` (or `cabal exec ghci Wn.hs`).
  This is a good idea for instance when you don't understand the
  test failures.

Solutions
---------

This repository also contains solutions to the exercises. You can get
the solutions for week `n` by running

    $ make WnSol.hs

The solutions are also visibile under the directory `templ/`. Don't look
there if you don't want spoilers!
