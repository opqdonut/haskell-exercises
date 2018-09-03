module W3 where

-- Week 3:
--   * built-in datatypes
--   * custom datatypes
--   * folds
--
-- Useful functions / types:
--   * Maybe

-- Ex 1: implement safe integer devision, that is, a function that
-- returns Just result normally, but Nothing if the divisor is zero.
--
-- Remember that integer division can be done with the div function.

safeDiv :: Integer -> Integer -> Maybe Integer
#ifdef sol
safeDiv x 0 = Nothing
safeDiv x y = Just (div x y)
#else
safeDiv x y = undefined
#endif

-- Ex 2: another variant of safe division. This time a succesful
-- division should be returned as
--   Right result
-- whereas a division by zero should return
--   Left "1234/0"
-- (replace 1234 with the value of x).

eitherDiv :: Integer -> Integer -> Either String Integer
#ifdef sol
eitherDiv x 0 = Left $ show x ++ "/0"
eitherDiv x y = Right $ div x y
#else
eitherDiv x y = undefined
#endif

-- Ex 3: implement the function mapMaybe, which works a bit like a
-- combined map & filter.
--
-- mapMaybe is given a list ([a]) and a function of type a -> Maybe b.
-- This function is called for all values in the list. If the function
-- returns Just x, x will be in the result list. If the function
-- returns Nothing, nothing gets added to the result list.
--
-- Examples:
--
-- let f x = if x>0 then Just (2*x) else Nothing
-- in mapMaybe f [0,1,-1,4,-2,2]
--   ==> [2,8,4]
--
-- mapMaybe Just [1,2,3]
--   ==> [1,2,3]
--
-- mapMaybe (\x -> Nothing) [1,2,3]
--   ==> []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
#ifdef sol
mapMaybe f [] = []
mapMaybe f (x:xs) = case f x of Just y -> y:mapMaybe f xs
                                Nothing -> mapMaybe f xs
#else
mapMaybe f xs = undefined
#endif

-- Ex 4: define the function classify that takes a list of Either a b
-- values and returns a list of the Left values and a list of the
-- Right values.
--
-- PS. This function can be found from the standard library under the
-- name partitionEithers. Don't use the library implementation or any
-- other functions from the Data.Maybe module.
--
-- Example:
--  classify [Left 1, Right True, Left 0, Right False]
--     ==> ([1,0],[True,False])

classify :: [Either a b] -> ([a],[b])
#ifdef sol
classify es = go es [] []
  where go (Left a : es) as bs = go es (a:as) bs
        go (Right b : es) as bs = go es as (b:bs)
        go [] as bs = (reverse as, reverse bs)
#else
classify es = undefined
#endif

-- Ex 5: define a datatype Person, which should contain the age (an
-- Int) and the name (a String) of a person.
--
-- Also define a Person value fred, and the functions getAge, getname,
-- setAge and setName (see below).

#ifdef sol
data Person = MkPerson Int String
  deriving Show
#else
data Person = PersonUndefined
  deriving Show
#endif

-- fred is a person whose name is Fred and age is 90
fred :: Person
#ifdef sol
fred = MkPerson 90 "Fred"
#else
fred = undefined
#endif

-- getName returns the name of the person
getName :: Person -> String
#ifdef sol
getName (MkPerson _ name) = name
#else
getName p = undefined
#endif

-- getAge returns the age of the person
getAge :: Person -> Int
#ifdef sol
getAge (MkPerson age _) = age
#else
getAge p = undefined
#endif

-- setName takes a person and returns a new person with the name changed
setName :: String -> Person -> Person
#ifdef sol
setName name (MkPerson age _name) = MkPerson age name
#else
setName name p = undefined
#endif

-- setAge does likewise for age
setAge :: Int -> Person -> Person
#ifdef sol
setAge age (MkPerson _age name) = MkPerson age name
#else
setAge age p = undefined
#endif


-- Ex 6: define a datatype TwoCounters which contains two Int
-- counters. Also define the functions below for operating a
-- TwoCounters.
--
-- Examples:
--
-- getA (incA (incA zeros))
--   ==> 2
-- getB (incB (incA zeros))
--   ==> 1

#ifdef sol
data TwoCounters = TwoCounters Int Int
#else
data TwoCounters = Undefined
#endif

-- zeros is a TwoCounters value with both counters initialized to 0
zeros :: TwoCounters
#ifdef sol
zeros = TwoCounters 0 0
#else
zeros = undefined
#endif

-- getA returns the value of the "A" counter
getA :: TwoCounters -> Int
#ifdef sol
getA (TwoCounters a _) = a
#else
getA tc = undefined
#endif

-- getB returns the value of the "B" counter
getB :: TwoCounters -> Int
#ifdef sol
getB (TwoCounters _ b) = b
#else
getB tc = undefined
#endif

-- incA increases the value of the "A" counter by one
incA :: TwoCounters -> TwoCounters
#ifdef sol
incA (TwoCounters a b) = TwoCounters (a+1) b
#else
incA tc = undefined
#endif

-- incB does likewise for the "B" counter
incB :: TwoCounters -> TwoCounters
#ifdef sol
incB (TwoCounters a b) = TwoCounters a (b+1)
#else
incB tc = undefined
#endif

-- Ex 7: define a datatype UpDown that represents a counter that can
-- either be in incresing or decreasing mode. Also implement the
-- functions zero, toggle, tick and get below.
--
-- NB! Define _two_ constructors for your datatype, in other words,
-- the definition should be of the form
--   data UpDown = A something | B foobar
--
-- Examples:
--
-- get (tick zero)
--   ==> 1
-- get (tick (tick zero))
--   ==> 2
-- get (tick (tick (toggle (tick zero))))
--   ==> -1

#ifdef sol
data UpDown = Up Int | Down Int
#else
data UpDown = UpDownUndefined1 | UpDownUndefined2
#endif

-- zero is an increasing counter with value 0
zero :: UpDown
#ifdef sol
zero = Up 0
#else
zero = undefined
#endif

-- get returns the counter value
get :: UpDown -> Int
#ifdef sol
get (Up x) = x
get (Down x) = x
#else
get ud = undefined
#endif

-- tick increases an increasing counter by one or decreases a
-- decreasing counter by one
tick :: UpDown -> UpDown
#ifdef sol
tick (Up x)   = Up (x+1)
tick (Down x) = Down (x-1)
#else
tick ud = undefined
#endif

-- toggle changes an increasing counter into a decreasing counter and
-- vice versa
toggle :: UpDown -> UpDown
#ifdef sol
toggle (Up x) = Down x
toggle (Down x) = Up x
#else
toggle ud = undefined
#endif

-- !!!!!
-- The next exercises use the binary tree type defined like this:

data Tree a = Leaf | Node a (Tree a) (Tree a)
            deriving (Show, Eq)

-- Ex 8: implement the function valAtRoot which returns the value at
-- the root (top-most node) of the tree. The return value is Maybe a
-- because the tree might be empty (i.e. just a Leaf)

valAtRoot :: Tree a -> Maybe a
#ifdef sol
valAtRoot Leaf = Nothing
valAtRoot (Node v _ _) = Just v
#else
valAtRoot t = undefined
#endif

-- Ex 9: compute the size of a tree, that is, the number of Node
-- constructors in it

treeSize :: Tree a -> Int
#ifdef sol
treeSize Leaf = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r
#else
treeSize t = undefined
#endif

-- Ex 10: get the leftmost value in the tree. The return value is
-- Maybe a because the tree might be empty.
--
-- The leftmost value means the value that is reached by going to the
-- left child of the current Node for as long as possible.
--
-- Examples:
--
-- leftest Leaf
--   ==> Nothing
-- leftest (Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) Leaf)
--   ==> Just 3
-- leftest (Node 1 (Node 2 Leaf (Node 3 Leaf Leaf)) (Node 4 Leaf Leaf))
--   ==> Just 2

leftest :: Tree a -> Maybe a
#ifdef sol
leftest Leaf = Nothing
leftest (Node x Leaf _) = Just x
leftest (Node _ l _) = leftest l
#else
leftest t = undefined
#endif

-- Ex 11: implement map for trees.
--
-- Examples:
--
-- mapTree (+1) Leaf  ==>  Leaf
-- mapTree (+2) (Node 0 (Node 1 Leaf Leaf) (Node 2 Leaf Leaf))
--   ==> (Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf))

mapTree :: (a -> b) -> Tree a -> Tree b
#ifdef sol
mapTree _ Leaf = Leaf
mapTree f (Node v l r) = Node (f v) (mapTree f l) (mapTree f r)
#else
mapTree f t = undefined
#endif

-- Ex 12: insert the given value into the leftmost possible place. You
-- need to return a new tree since the function is pure.
--
-- Example:
-- insertL 0 Leaf
--   ==> Node 0 Leaf Leaf
-- insertL 0 (Node 1 Leaf Leaf)
--   ==> Node 1 (Node 0 Leaf Leaf) Leaf)
--
-- insertL 0 (Node 1
--             (Node 2
--               Leaf
--               (Node 3 Leaf Leaf))
--             (Node 4 Leaf Leaf))
--
--        ==> Node 1
--             (Node 2
--               (Node 0 Leaf Leaf)
--               (Node 3 Leaf Leaf))
--             (Node 4 Leaf Leaf)


insertL :: a -> Tree a -> Tree a
#ifdef sol
insertL x Leaf = Node x Leaf Leaf
insertL x (Node y l r) = Node y (insertL x l) r
#else
insertL x t = undefined
#endif

-- Ex 13: implement the function measure, that takes a tree and
-- returns a tree with the same shape, but with the value at every
-- node being the size of the subtree starting at that node.
--
-- (If you don't remember what the size of a subtree meant, see Ex 9)
--
-- Examples:
--
-- measure (Node 'a' Leaf Leaf)
--  ==> Node 1 Leaf Leaf
-- measure (Node 'a' (Node 'b' Leaf Leaf) Leaf)
--  ==> Node 2 (Node 1 Leaf Leaf) Leaf
-- measure (Node 0 (Node 0 Leaf Leaf) Leaf)
--  ==> Node 2 (Node 1 Leaf Leaf) Leaf
-- measure (Node 0 (Node 0 Leaf Leaf)
--                 (Node 0 (Node 0 Leaf Leaf)
--                         (Node 0 Leaf
--                                 (Node 0 Leaf Leaf))))
--      ==> Node 6 (Node 1 Leaf Leaf)
--                 (Node 4 (Node 1 Leaf Leaf)
--                         (Node 2 Leaf
--                                 (Node 1 Leaf Leaf)))


measure :: Tree a -> Tree Int
#ifdef sol
measure Leaf = Leaf
measure (Node _ l r) = Node (val l' + val r' + 1) l' r'
  where val Leaf = 0
        val (Node v _ _) = v
        l' = measure l
        r' = measure r
#else
measure t = undefined
#endif

-- Ex 14: the standard library function
--   foldr :: (a -> b -> b) -> b -> [a] -> b
-- is used to "collapse" a list to a single value, like this:
--   foldr f start [x,y,z,w]
--     ==> f x (f y (f z (f w start)
--
-- Implement the functions sumf and lengthf so that mysum computes the
-- sum of the values in the list and mylength computes the length of
-- the list.
--
-- DON'T change the definitions of mysum and mylength, only implement
-- sumf and lengtf appropriately.

mysum :: [Int] -> Int
mysum is = foldr sumf 0 is

sumf :: Int -> Int -> Int
#ifdef sol
sumf x y = x + y
#else
sumf x y = undefined
#endif

mylength :: [a] -> Int
mylength xs = foldr lengthf 0 xs

lengthf :: a -> Int -> Int
#ifdef sol
lengthf x y = 1 + y
#else
lengthf x y = undefined
#endif

-- Ex 15: implement the function foldTree that works like foldr, but
-- for Trees.
--
-- Example:
--   foldTree f l (Node 3 Leaf Leaf)
--     ==> f 3 l l
--   foldTree f l (Node 'a' (Node 'b' (Node 'c' Leaf Leaf)
--                                    Leaf)
--                          (Node 'd' Leaf Leaf))
--     ==> f (f 'a' (f 'b' (f 'c' l l)
--                         l)
--                  (f 'd' l l))
--
-- Once you've implemented foldTree correctly, the functions treeSum
-- and treeLeaves below work correctly.

sumt :: Int -> Int -> Int -> Int
sumt x y z = x+y+z

-- Sum of numbers in the tree
treeSum :: Tree Int -> Int
treeSum t = foldTree sumt 0 t

leaft :: a -> Int -> Int -> Int
leaft x y z = y+z

-- Number of leaves in the tree
treeLeaves :: Tree a -> Int
treeLeaves t = foldTree leaft 1 t

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
#ifdef sol
foldTree f x Leaf = x
foldTree f x (Node val l r) = f val (foldTree f x l) (foldTree f x r)
#else
foldTree f x t = undefined
#endif

-- Ex 16: You'll find a Color datatype below. It has the three basic
-- colours Red, Green and Blue, and two color transformations, Mix and
-- Darken.
--
-- Mix means the sum of the two colors.
--
-- Darken means darkening the color. The Double value tells how much
-- the color is darkened. 0.0 means no change and 1.0 means full
-- darkening, i.e. black.
--
-- Implement the function rgb :: Color -> [Double] that returns a list
-- of length three that represents the rgb value of the given color.
--
-- Examples:
--
-- rgb Red   ==> [1,0,0]
-- rgb Green ==> [0,1,0]
-- rgb Blue  ==> [0,0,1]
--
-- rgb (Mix Red Green)                ==> [1,1,0]
-- rgb (Mix Red (Mix Red Green))      ==> [1,1,0]
-- rgb (Darken 0.2 Red)               ==> [0.8,0,0]
-- rgb (Darken 0.2 (Darken 0.2 Red))  ==> [0.64,0,0]
-- rgb (Mix (Darken 0.4 Red) (Darken 0.4 Red)) ==> [1,0,0]
-- rgb (Mix (Darken 0.6 Red) (Darken 0.6 Red)) ==> [0.8,0,0]
--
-- NB! Mix should saturate at 1.0

data Color = Red | Green | Blue | Mix Color Color | Darken Double Color
  deriving Show

rgb :: Color -> [Double]
#ifdef sol
rgb Red = [1,0,0]
rgb Green = [0,1,0]
rgb Blue = [0,0,1]
rgb (Mix c c') = map saturate $ zipWith (+) (rgb c) (rgb c')
  where saturate x = min x 1
rgb (Darken d c) = map (*scale) (rgb c)
  where scale = 1-d
#else
rgb col = undefined
#endif
