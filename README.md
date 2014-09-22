Opqdonut's Haskell Exercises
============================

Quick Start
-----------

First, generate the exercise templates (files W*.hs):

    $ make

Then check you can actually run the tests with:

    $ runhaskell W0Test.hs

This should print "Everything seems to be OK!". If you see any errors,
you might not have the QuickCheck library installed.

Now you can edit W1.hs and see how well you did by running

    $ runhaskell W1Test.hs

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
* more weeks TODO

Solutions
---------

This repository also contains solutions to the exercises. You can get
the solutions for week n by running

    $ make WnSol.hs

The solutions are also visibile under the directory templ/. Don't look
there if you don't want spoilers!
