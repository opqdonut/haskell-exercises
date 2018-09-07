module W5 where

import System.Random
import Data.List

-- Week 5:
--  - operators
--  - using typeclasses
--  - implementing typeclasses
--  - forcing/laziness
--
-- Useful type classes to know:
--  - Eq
--  - Ord
--  - Show
--  - Num
--  - Functor

-- Ex 1: hey, did you know you can implement your own operators in
-- Haskell? Implement the operator %$ that combines two strings like
-- this:
--
-- "aa" %$ "foo" ==> "aafooaa"
--
-- and the operator *! that takes a value and a number and produces a
-- list that repeats the value that many times:
--
-- True *! 3 ==> [True,True,True]

(%$) :: String -> String -> String
#ifdef sol
x %$ y = x ++ y ++ x
#else
x %$ y = undefined
#endif

(*!) :: Int -> a -> [a]
#ifdef sol
n *! val = replicate n val
#else
n *! val = undefined
#endif

-- Ex 2: implement the function allEqual which returns True if all
-- values in the list are equal.
--
-- Examples:
--
-- allEqual [] ==> True
-- allEqual [1,2,3] ==> False
-- allEqual [1,1,1] ==> True
--
-- PS. check out the error message you get with your implementation if
-- you remove the Eq a => constraint from the type!

allEqual :: Eq a => [a] -> Bool
#ifdef sol
allEqual [] = True
allEqual (x:xs) = all (==x) xs
#else
allEqual xs = undefined
#endif

-- Ex 3: implement the function secondSmallest that returns the second
-- smallest value in the list, or Nothing if there is no such value.
--
-- Examples:
--
-- secondSmallest [1.0] ==>  Nothing
-- secondSmallest [1,1] ==>  Just 1
-- secondSmallest [5,3,7,2,3,1]  ==>  Just 2

secondSmallest :: Ord a => [a] -> Maybe a
#ifdef sol
secondSmallest [] = Nothing
secondSmallest [x] = Nothing
secondSmallest xs = Just v
  where m = minimum xs
        xs' = delete m xs
        v = minimum xs'
#else
secondSmallest xs = undefined
#endif

-- Ex 4: find how two lists differ from each other. If they have
-- different lengths, return
--   Just "<length of list> /= <length of other list>"
-- if they have the same length, find the first index i for which the
-- elements differ, and return
--   Just "<value at index i> /= <other value at index i>"
-- if the lists are the same, return
--   Nothing
--
-- NB! Write the type signature for findDifference your self. Which
-- type classes do you need?
--
-- Examples:
--  findDifference [True,False] [True,True]
--    ==> Just "False /= True"
--  findDifference [0,0,0] [0,0,0,0]
--    ==> Just "3 /= 4"

#ifdef sol
findDifference :: (Eq a, Show a) => [a] -> [a] -> Maybe String
findDifference xs ys
  | lx/=ly = Just (show lx ++ " /= " ++ show ly)
  | otherwise = go xs ys
  where lx = length xs
        ly = length ys
        go [] [] = Nothing
        go (x:xs) (y:ys)
          | x/=y   = Just (show x ++ " /= " ++ show y)
          | otherwise = findDifference xs ys
#else
findDifference = undefined
#endif

-- Ex 5: compute the average of a list of values of the Fractional
-- class.
--
-- Hint! since Fractional is a subclass of Num, you have all
-- arithmetic operations available
--
-- Hint! you can use the function fromIntegral to convert the list
-- length to a Fractional

average :: Fractional a => [a] -> a
#ifdef sol
average xs = sum xs / fromIntegral (length xs)
#else
average xs = undefined
#endif

-- Ex 6: define an Eq instance for the type Foo below.

data Foo = Bar | Quux | Xyzzy
  deriving Show

instance Eq Foo where
#ifdef sol
  Bar == Bar     = True
  Quux == Quux   = True
  Xyzzy == Xyzzy = True
  _ == _         = False
#else
  (==) = error "implement me"
#endif

-- Ex 7: implement an Ord instance for Foo so that Quux < Bar < Xyzzy

instance Ord Foo where
#ifdef sol
  Quux <= Bar   = True
  Bar <= Xyzzy  = True
  Quux <= Xyzzy = True
  x <= y        = x == y
#else
  compare = error "implement me?"
  (<=) = error "and me?"
  min = error "and me?"
  max = error "and me?"
#endif

-- Ex 8: here is a type for a 3d vector. Implement an Eq instance for it.

data Vector = Vector Integer Integer Integer
  deriving Show

instance Eq Vector where
#ifdef sol
  (Vector a b c) == (Vector a' b' c')  = a==a' && b==b' && c==c'
#else
  (==) = error "implement me"
#endif

-- Ex 9: implementa Num instance for Vector such that all the
-- arithmetic operations work componentwise.
--
-- You should probably check the docs for which methods Num has!
--
-- Examples:
--
-- Vector 1 2 3 + Vector 0 1 1 ==> Vector 1 3 4
-- Vector 1 2 3 * Vector 0 1 2 ==> Vector 0 2 6
-- abs (Vector (-1) 2 (-3))    ==> Vector 1 2 3
-- signum (Vector (-1) 2 (-3)) ==> Vector (-1) 1 (-1)

instance Num Vector where
#ifdef sol
  Vector a b c + Vector a' b' c' = Vector (a+a') (b+b') (c+c')
  Vector a b c * Vector a' b' c' = Vector (a*a') (b*b') (c*c')
  signum (Vector a b c) = Vector (signum a) (signum b) (signum c)
  negate (Vector a b c) = Vector (negate a) (negate b) (negate c)
  abs (Vector a b c) = Vector (abs a) (abs b) (abs c)
  fromInteger x = Vector x x x
#else
#endif

-- Ex 10: compute how many times each value in the list occurs. Return
-- the frequencies as a list of (frequency,value) pairs.
--
-- Hint! feel free to use functions from Data.List
--
-- Example:
-- freqs [False,False,False,True]
--   ==> [(3,False),(1,True)]

freqs :: Eq a => [a] -> [(Int,a)]
#ifdef sol
freqs [] = []
freqs (x:xs) = (count+1,x) : freqs left
  where (equals,left) = partition (==x) xs
        count = length equals
#else
freqs xs = undefined
#endif

-- Ex 11: implement an Eq instance for the following binary tree type

data ITree = ILeaf | INode Int ITree ITree
  deriving Show

instance Eq ITree where
#ifdef sol
  ILeaf == ILeaf               = True
  INode x l r == INode y l' r' = x==y && l==l' && r==r'
  _ == _                       = False
#else
  (==) = error "implement me"
#endif

-- Ex 12: here is a list type parameterized over the type it contains.
-- Implement an instance "Eq a => Eq (List a)" that compares elements
-- of the lists.

data List a = Empty | LNode a (List a)
  deriving Show

instance Eq a => Eq (List a) where
#ifdef sol
  Empty == Empty            = True
  Empty == _                = False
  LNode x xs == LNode y ys  = x == y && xs == ys
  _ == _                    = False
#else
  (==) = error "implement me"
#endif

-- Ex 13: start by reading a bit about Functors. A Functor is a thing
-- you can "map" over, e.g. lists, Maybes.
--
-- Implement the function incrementAll that takes a functorial value
-- and increments each number inside by one.
--
-- Examples:
--   incrementAll [1,2,3]     ==>  [2,3,4]
--   incrementAll (Just 3.0)  ==>  Just 4.0

incrementAll :: (Functor f, Num n) => f n -> f n
#ifdef sol
incrementAll x = fmap (+1) x
#else
incrementAll x = undefined
#endif

-- Ex 14: below you'll find a type Result that works a bit like Maybe,
-- but there are two different types of "Nothings": one with and one
-- without an error description.
--
-- Implement the instance Functor Result

data Result a = MkResult a | NoResult | Failure String
  deriving (Show,Eq)

instance Functor Result where
#ifdef sol
  fmap f (MkResult x) = MkResult (f x)
  fmap _ NoResult = NoResult
  fmap _ (Failure s) = (Failure s)
#else
  fmap f result = error "implement me"
#endif

-- Ex 15: Implement the instance Functor List (for the datatype List
-- from ex 12)

instance Functor List where
#ifdef sol
  fmap f Empty = Empty
  fmap f (LNode x xs) = LNode (f x) (fmap f xs)
#else
#endif

-- Ex 16: Fun a is a type that wraps a function Int -> a. Implement a
-- Functor instance for it.
--
-- Figuring out what the Functor instance should do is most of the
-- puzzle.

data Fun a = Fun (Int -> a)

runFun :: Fun a -> Int -> a
runFun (Fun f) x = f x

instance Functor Fun where
#ifdef sol
  -- An explanation in case the definition below puzzles you
  --
  -- In this case fmap :: (a->b) -> Fun a -> Fun b
  -- so the definition should looklike this: fmap f (Fun g) = Fun h
  -- where f :: a->b, g :: Int->a, h :: Int->b
  --
  -- Let's define h. h is a function that takes an int, so
  --   h i = ....
  -- h returns something of type b, and the only way to produce
  -- something of type b is the function f, so
  --   h i = f (...)
  -- f takes an a, and the only way we can get an a is g, so
  --   h i = f (g (...))
  -- g takes an int, and i is an int
  --   h i = f (g i)
  -- we can rewrite this as
  --   h = f . g
  -- Thus:
  fmap f (Fun g) = Fun (f.g)
#else
#endif

-- Ex 17: this and the next exercise serve as an introduction for the
-- next week.
--
-- The module System.Random has the typeclass RandomGen that
-- represents a random generator. The class Random is for values that
-- can be randomly generated by RandomGen.
--
-- The relevant function in System.Random is
--   random :: (Random a, RandomGen g) => g -> (a, g)
-- that takes a random generator and returns a random value, and the
-- new state of the generator (remember purity!)
--
-- Implement the function threeRandom that generates three random
-- values. You don't need to return the final state of the random
-- generator (as you can see from the return type).
--
-- NB! if you use the same generator multiple times, you get the same
-- output. Remember to use the new generator returned by random.
--
-- NB! the easiest way to get a RandomGen value is the function
-- mkStdGen that takes a seed and returns a random generator.
--
-- Examples:
--  *W5> threeRandom (mkStdGen 1) :: (Int,Int,Int)
--  (7917908265643496962,-1017158127812413512,-1196564839808993555)
--  *W5> threeRandom (mkStdGen 2) :: (Bool,Bool,Bool)
--  (True,True,False)

threeRandom :: (Random a, RandomGen g) => g -> (a,a,a)
#ifdef sol
threeRandom g = (x,y,z)
  where (x,g1) = random g
        (y,g2) = random g1
        (z,_)  = random g2
#else
threeRandom g = undefined
#endif

-- Ex 18: given a Tree (same type as on Week 3), randomize the
-- contents of the tree.
--
-- That is, you get a RandomGen and a Tree, and you should return a
-- Tree with the same shape, but random values in the Nodes.
--
-- This time you should also return the final state of the RandomGen
--
-- Hint! the recursive solution is straightforward, but requires
-- careful threading of the RandomGen versions.
--
-- Examples:
--  *W5> randomizeTree (Node 0 (Node 0 Leaf Leaf) Leaf) (mkStdGen 1)  :: (Tree Char, StdGen)
--  (Node '\603808' (Node '\629073' Leaf Leaf) Leaf,1054756829 1655838864)
--  *W5> randomizeTree (Node True Leaf Leaf) (mkStdGen 2)  :: (Tree Int, StdGen)
--  (Node (-2493721835987381530) Leaf Leaf,1891679732 2103410263)


data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

randomizeTree :: (Random a, RandomGen g) => Tree b -> g -> (Tree a,g)
#ifdef sol
randomizeTree Leaf g = (Leaf,g)
randomizeTree (Node _ l r) g = (Node x l' r', g3)
  where (l',g1) = randomizeTree l g
        (r',g2) = randomizeTree r g1
        (x,g3)  = random g2
#else
randomizeTree t g = undefined
#endif
