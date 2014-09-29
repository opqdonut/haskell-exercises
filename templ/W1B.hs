module W1 where

-- Week 1:
--   * defining functions
--   * basic expressions
--   * pattern matching
--   * recursion

-- Ex 1: define variables one, two and three. They should all have
-- type Int and values 1, 2 and 3. This exercise has no tests.

-- Ex 2: define the function double of type Integer->Integer. Double
-- should take one argument and return it multiplied by two.

double :: Integer -> Integer
#ifdef sol
double x = 2*x
#else
double x = undefined
#endif

-- Ex 3: define the function quadruple that uses the function double
-- from the previous exercise to return its argument multiplied by
-- four.

quadruple :: Integer -> Integer
#ifdef sol
quadruple x = double (double x)
#else
quadruple x = undefined
#endif

-- Ex 4: define the function poly2. It should take four arguments of
-- type Double, a, b, c, and x and return a*x^2+b*x+c. Give poly2 a
-- type signature, i.e. poly2 :: something.

#ifdef sol
poly2 :: Double -> Double -> Double -> Double -> Double
poly2 a b c x = a*x*x+b*x+c
#else
poly2 = undefined
#endif

-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.
--
-- Ps. have a look at the built in function "even"

eeny :: Integer -> String
#ifdef sol
eeny i = if even i then "eeny" else "meeny"
#else
eeny = undefined
#endif

-- Ex 6: fizzbuzz! Define the a function fizzbuzz that returns "Fizz"
-- for numbers divisible by 3, "Buzz" for numbers divisible by 5, and
-- "FizzBuzz" for numbers divisible by both. For other numbers it
-- returns the empty string.
--
-- You can use the function mod to compute modulo.

#ifdef sol
fizzbuzz :: Integer -> String
fizzbuzz n = if divides 3 n && divides 5 n
             then "FizzBuzz"
             else if divides 3 n
                  then "Fizz"
                  else if divides 5 n
                       then "Buzz"
                       else ""
  where divides k n = mod n k == 0
#else
fizzbuzz = undefined
#endif

-- Ex 7: define a function isZero that returns True if it is given an
-- Integer that is 0, and False otherwise. Give isZero a type signature.
--
-- Use pattern matching! Don't use comparisons!
--
-- Ps. the type of booleans in haskell is Bool

#ifdef sol
isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False
#else
isZero = undefined
#endif

-- Ex 8: implement using recursion a function sumTo such that
--   sumTo n
-- computes the sum 1+2+...+n

sumTo :: Integer -> Integer
#ifdef sol
sumTo 1 = 1
sumTo n = n + sumTo (n-1)
#else
sumTo = undefined
#endif

-- Ex 9: power n k should compute n to the power k (i.e. n^k)
-- Use recursion.

power :: Integer -> Integer -> Integer
#ifdef sol
power n 0 = 1
power n k = n * power n (k-1)
#else
power = undefined
#endif

-- Ex 10: ilog2 n should be the number of times you can halve the
-- integer n (rounding down) before you get 1.
--
-- Use recursion to define ilog2. Use the function "div" for integer
-- division.

ilog2 :: Integer -> Integer
#ifdef sol
ilog2 1 = 0
ilog2 n = 1 + ilog2 (div n 2)
#else
ilog2 = undefined
#endif

-- Ex 11: compute binomial coefficients using recursion. Binomial
-- coefficients are defined by the following equations:
--
--   B(n,k) = B(n-1,k) + B(n-1,k-1)
--   B(n,0) = 1
--   B(0,k) = 0, when k>0
--
-- Hint! pattern matching is your friend.

binomial :: Integer -> Integer -> Integer
#ifdef sol
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)
#else
binomial = undefined
#endif

-- Ex 12: The tribonacci numbers are defined by the equations
--
--   T(1) = 1
--   T(2) = 1
--   T(3) = 2
--   T(n+1) = T(n)+T(n-1)+T(n-2)
--
-- Implement an efficient, linear time, recursive function that
-- computes T(n). You'll probably want to define a helper function.

tribonacci :: Integer -> Integer
#ifdef sol
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci n = tribonacci' 1 1 2 (n-2)

tribonacci' :: Integer -> Integer -> Integer -> Integer -> Integer
tribonacci' a b c 1 = c
tribonacci' a b c n = tribonacci' b c (a+b+c) (n-1)
#else
tribonacci = undefined
#endif

-- Ex 13: implement the euclidean algorithm for finding the greatest
-- common divisor: http://en.wikipedia.org/wiki/Euclidean_algorithm

myGcd :: Integer -> Integer -> Integer
#ifdef sol
myGcd 0 y = y
myGcd x y = if x<y
            then myGcd y x
            else myGcd (x-y) y
#else
myGcd = undefined
#endif

-- Ex 14: The Haskell Prelude (standard library) defines the type
-- Ordering with values LT, GT and EQ. You try out Ordering by
-- evaluating the following expressions:
--
--   compare 3 4
--   compare 4 3
--   compare 0 0
--   compare "Hei" "Moi"
--   compare True False
--
-- Your task is to implement the function funnyCompare, that orders
-- integers in the following way:
--
-- 1. All even numbers come before odd numbers
-- 2. Within even and odd numbers the ordering is normal

funnyCompare :: Int -> Int -> Ordering
#ifdef sol
funnyCompare x y = if even x
                   then if even y
                        then compare x y
                        else LT
                   else if even y
                        then GT
                        else compare x y
#else
funnyCompare = undefined
#endif

-- Ex 15: Implement the function funnyMin that returns the minimum of
-- its two arguments, according to the ordering implemented by
-- funnyCompare.
--
-- Use pattern matching on the Ordering value returned by
-- funnyCompare. To do this, you need to either use the case-of
-- expression or define a helper function.

funnyMin :: Int -> Int -> Int
#ifdef sol
funnyMin x y = helper (funnyCompare x y) x y
  where helper LT x _ = x
        helper _  _ y = y
#else
funnyMin = undefined
#endif

-- Ex 16: implement the recursive function pyramid that returns
-- strings like this:
--
-- pyramid 0 ==> "0"
-- pyramid 1 ==> "0,1,0"
-- pyramid 2 ==> "0,1,2,1,0"
-- pyramid 3 ==> "0,1,2,3,2,1,0"
--
-- Hints:
-- * you can glue strings together with the operator ++
-- * the function show transforms a number into a string
-- * you'll need a (recursive) helper function

pyramid :: Integer -> String
#ifdef sol
pyramid n = helper 0 n

helper k 0 = show k
helper k n = show k ++ "," ++ helper (k+1) (n-1) ++ "," ++ show k
#else
pyramid = undefined
#endif

-- Ex 17: implement the function smallestDivisor that returns the
-- smallest number (greater than 1) that divides the given number.
--
-- That is, when
--   smallestDivisor n ==> k
-- we have
--   n = t*k
-- for some t.
--
-- Ps. your function doesn't need to work for inputs 0 and 1, but
-- remember this in the next exercise!

smallestDivisor :: Integer -> Integer
#ifdef sol
smallestDivisor n = smallestDivisor' 2 n

smallestDivisor' k n =
  if mod n k == 0
  then k
  else smallestDivisor' (k+1) n
#else
smallestDivisor = undefined
#endif

-- Ex 18: implement a function isPrime that checks if the given number
-- is a prime number. Use the function smallestDivisor.
--
-- Ps. 0 and 1 are not prime numbers

isPrime :: Integer -> Bool
#ifdef sol
isPrime 0 = False
isPrime 1 = False
isPrime i = smallestDivisor i == i
#else
isPrime = undefined
#endif

-- Ex 19: implement a function nextPrime that returns the first prime
-- number that comes after the given number. Use the function isPrime
-- you just defined.

nextPrime :: Integer -> Integer
#ifdef sol
nextPrime n =
  if isPrime (n+1)
  then (n+1)
  else nextPrime (n+1)
#else
nextPrime = undefined
#endif
