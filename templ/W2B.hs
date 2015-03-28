module W2 where

-- Week 2:
--
--  * lists
--  * strings
--  * library functions for them
--  * higher order functions
--  * polymorphism
--
-- Functions you will need:
--  * head, tail
--  * take, drop
--  * length
--  * null
--  * map
--  * filter
--
-- You can ask ghci for the types of these functions with the :t
-- command:
--
--  Prelude> :t length
--  length :: [a] -> Int

import Data.List
import Data.Char

-- Ex 1: Define the constant years, that is a list of the values 1982,
-- 2004 and 2012 in this order.

#ifdef sol
years = [1982,2004,2012]
#else
years = undefined
#endif

-- Ex 2: define the function measure that for an empty list returns -1
-- and for other lists returns the length of the list.

measure :: [String] -> Int
#ifdef sol
measure ss = if null ss then -1 else length ss
#else
measure ss = undefined
#endif

-- Ex 3: define the function takeFinal, which returns the n last
-- elements of the given list.

takeFinal :: Int -> [Int] -> [Int]
#ifdef sol
takeFinal n xs = drop (length xs - n) xs
#else
takeFinal n xs = undefined
#endif

-- Ex 4: remove the nth element of the given list. More precisely,
-- return a list that is identical to the given list except the nth
-- element is missing.
--
-- Note! indexing starts from 0
--
-- Examples:
-- remove 0 [1,2,3]    ==>  [2,3]
-- remove 2 [4,5,6,7]  ==>  [4,5,7]
--
-- The [a] in the type signature means "a list of any type"

remove :: Int -> [a] -> [a]
#ifdef sol
remove i xs = take i xs ++ drop (i+1) xs
#else
remove i xs = undefined
#endif

-- Ex 5: substring i n s should return the length n substring of s
-- starting at index i.
--
-- Remember that strings are lists!

substring :: Int -> Int -> String -> String
#ifdef sol
substring i n s = take n (drop i s)
#else
substring i n s = undefined
#endif

-- Ex 6: implement the function mymax that takes as argument a
-- measuring function (of type a -> Int) and two values (of type a).
--
-- mymax should apply the measuring function to both arguments and
-- return the argument for which the measuring function returns a
-- higher value.
--
-- Examples:
--
--  mymax (*2)   3       5      ==>  5
--  mymax length [1,2,3] [4,5]  ==>  [1,2,3]
--  mymax head   [1,2,3] [4,5]  ==>  [4,5]

mymax :: (a -> Int) -> a -> a -> a
#ifdef sol
mymax measure a b
  | measure a > measure b = a
  | otherwise             = b
#else
mymax measure a b = undefined
#endif

-- Ex 7: countSorted receives a list of strings and returns a count of
-- how many of the strings are in alphabetical order (i.e. how many of
-- the strings have their letters in alphabetical order)
--
-- Remember the functions length, filter and sort

countSorted :: [String] -> Int
#ifdef sol
countSorted ss = length $ filter sorted ss
  where sorted s = sort s == s
#else
countSorted ss = undefined
#endif

-- Ex 8: Implement a function funny, that
--  - takes in a list of strings
--  - returns a string
--    - that contains all input words of length over 5
--    - ... combined into one string
--    - ... separated with spaces
--    - ... and converted to upper case!
--
-- These functions will help:
--  - toUpper :: Char -> Char   from the module Data.Char
--  - intercalate               from the module Data.List

funny :: [String] -> String
#ifdef sol
funny strings = map toUpper . intercalate " " . filter (\x -> length x > 5) $ strings
#else
funny strings = undefined
#endif

-- Ex 9: implement quicksort. Quicksort is a recursive sorting
-- algorithm that works like this.
--
--  - The empty list is the base case of the recursion: it is already sorted
--  - From a nonempty list, the first element is chosen to be the "pivot", and
--    - the elements smaller than pivot are gathered into a list
--    - the elements smaller than larger or equal to the pivot are gathered
--    - these two lists are sorted using recursion
--    - finally the small elements, the pivot and the large elements
--      are combined into one sorted list
--
-- PS. yes if you want to nit-pick this isn't really quicksort :)

quicksort :: [Int] -> [Int]
#ifdef sol
quicksort xs
  | null xs   = xs
  | otherwise = let pivot = head xs
                    rest = tail xs
                    smaller = filter (<pivot) rest
                    bigger = filter (>=pivot) rest
                in quicksort smaller ++ [pivot] ++ quicksort bigger
#else
quicksort xs = undefined
#endif

-- Ex 10: powers k max should return all the powers of k that are less
-- than or equal to max. For example:
--
-- powers 2 5 ==> [1,2,4]
-- powers 3 30 ==> [1,3,9,27]
-- powers 2 2 ==> [1,2]
--
-- Hints:
--   * n^max > max
--   * the function takeWhile

powers :: Int -> Int -> [Int]
#ifdef sol
powers n max = takeWhile (<=max) $ map (n^) [0..max]
#else
powers n max = undefined
#endif

-- Ex 11: implement a search function that takes an updating function,
-- a checking function and an initial value. Search should repeatedly
-- apply the updating function to the initial value until a value is
-- produced that passes the checking function. This value is then
-- returned.
--
-- Examples:
--
--   search (+1) even 0   ==>   0
--
--   search (+1) (>4) 0   ==>   5
--
--   let check [] = True
--       check ('A':xs) = True
--       check _ = False
--   in search tail check "xyzAvvt"
--     ==> Avvt

search :: (a->a) -> (a->Bool) -> a -> a
#ifdef sol
search update check initial
  | check initial = initial
  | otherwise = search update check (update initial)
#else
search update check initial = undefined
#endif

-- Ex 12: given numbers n and k, build the list of numbers n,n+1..k.
-- Use recursion and the : operator to build the list.

fromTo :: Int -> Int -> [Int]
#ifdef sol
fromTo n k = if n>k then [] else n:fromTo (n+1) k
#else
fromTo n k = undefined
#endif

-- Ex 13: given i, build the list of sums [1, 1+2, 1+2+3, .., 1+2+..+i]
--
-- Ps. you'll probably need a recursive helper function

sums :: Int -> [Int]
#ifdef sol
sums i = go 1 2
  where go acc n = if n>i then [acc] else acc : go (acc+n) (n+1)
#else
sums i = undefined
#endif

-- Ex 14: using list pattern matching and recursion, define a function
-- mylast that returns the last value of the given list. For an empty
-- list, a provided default value is returned.
--
-- Examples:
--   mylast 0 [] ==> 0
--   mylast 0 [1,2,3] ==> 3

mylast :: a -> [a] -> a
#ifdef sol
mylast def []     = def
mylast _   (x:xs) = mylast x xs
#else
mylast def xs = undefined
#endif

-- Ex 15: define a function that checks if the given list is in
-- increasing order. Use recursion and pattern matching. Don't use any
-- library list functions.

sorted :: [Int] -> Bool
#ifdef sol
sorted []  = True
sorted [x] = True
sorted (x:y:xs)
  | x>y       = False
  | otherwise = sorted (y:xs)
#else
sorted xs = undefined
#endif

-- Ex 16: compute the partial sums of the given list like this:
--
--   sumsOf [a,b,c]  ==>  [a,a+b,a+b+c]
--   sumsOf [a,b]    ==>  [a,a+b]
--   sumsOf []       ==>  []

sumsOf :: [Int] -> [Int]
#ifdef sol
sumsOf xs = go 0 xs
  where go acc (x:xs) = (acc+x) : go (acc+x) xs
        go _   [] = []
#else
sumsOf xs = undefined
#endif

-- Ex 17: define the function mymaximum that takes a list and a
-- comparing function of type a -> a -> Ordering and returns the
-- maximum value of the list, according to the comparing function.
--
-- For an empty list the given default value is returned.
--
-- Examples:
--   mymaximum compare (-1) [] ==> -1
--   mymaximum compare (-1) [1,3,2] ==> 3
--   let comp 0 0 = EQ
--       comp _ 0 = LT
--       comp 0 _ = GT
--       comp x y = compare x y
--   in mymaximum comp 1 [1,4,6,100,0,3]
--     ==> 0

mymaximum :: (a -> a -> Ordering) -> a -> [a] -> a
#ifdef sol
mymaximum cmp def [] = def
mymaximum cmp _   (x:xs) = go x xs
  where go biggest []     = biggest
        go biggest (x:xs)
          | cmp x biggest == GT   = go x xs
          | otherwise             = go biggest xs
#else
mymaximum cmp def xs = undefined
#endif

-- Ex 18: define a version of map that takes a two-argument function
-- and two lists. Example:
--   map2 f [x,y,z,w] [a,b,c]  ==> [f x a, f y b, f z c]
--
-- Use recursion and pattern matching.
--
-- Ps. this function is in the Haskell Prelude but under a different
-- name.

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
#ifdef sol
map2 f (a:as) (b:bs) = f a b:map2 f as bs
map2 f _      _      = []
#else
map2 f as bs = undefined
#endif

-- Ex 19: in this exercise you get to implement an interpreter for a
-- simple language. The language controls two counters, A and B, and
-- has the following commands:
--
-- incA -- increment counter A by one
-- incB -- likewise for B
-- decA -- decrement counter A by one
-- decB -- likewise for B
-- printA -- print value in counter A
-- printB -- print value in counter B
--
-- The interpreter will be a function of type [String] -> [String].
-- Its input is a list of commands, and its output is a list of the
-- results of the print commands in the input.
--
-- Both counters should start at 0.
--
-- Examples:
--
-- interpreter ["incA","incA","incA","printA","decA","printA"] ==> ["3","2"]
-- interpreter ["incA","incB","incB","printA","printB"] ==> ["1","2"]
--
-- Surprise! after you've implemented the function, try running this in GHCi:
--     interact (unlines . interpreter . lines)
-- after this you can enter commands on separate lines and see the
-- responses to them
--
-- Unfortunately the surprise might not work if you've implemented
-- your interpreter correctly but weirdly :(

interpreter :: [String] -> [String]
#ifdef sol
interpreter commands = go 0 0 commands
  where go a b ("incA":commands) = go (a+1) b commands
        go a b ("decA":commands) = go (a-1) b commands
        go a b ("incB":commands) = go a (b+1) commands
        go a b ("decB":commands) = go a (b-1) commands
        go a b ("printA":commands) = show a : go a b commands
        go a b ("printB":commands) = show b : go a b commands
        go a b []                  = []
        go a b (_:commands)        = "BAD" : go a b commands
#else
interpreter commands = undefined
#endif

-- Ex 20: write a function that finds the n first squares (numbers of
-- the form x*x) that start and end with the same digit.
--
-- Example: squares 9 ==> [1,4,9,121,484,676,1521,1681,4624]
--
-- Remember, the function show transforms a number to a string.

squares :: Int -> [Integer]
#ifdef sol
squares n = take n . filter (\x -> head (show x) == last (show x)) $ map (\x -> x*x) [1..]
#else
squares n = undefined
#endif
