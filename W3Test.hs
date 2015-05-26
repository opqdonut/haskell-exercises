module W3Test where

import W3
import Impl.Test

import Data.Either
import Control.Monad
import Test.QuickCheck hiding (Result,reason,classify,(===))

main = testExs tests

tests = [[property $ ex1_safeDiv_zero, property $ ex1_safeDiv_nonZero]
        ,[property $ ex2_eitherDiv_zero
         ,property $ ex2_eitherDiv_nonZero]
        ,[property $ ex3_mapMaybe_1, property $ ex3_mapMaybe_2]
        ,[property $ ex4_classify]
        ,[ex5_fred, ex5_age, ex5_name]
        ,[ex6_TwoCounters]
        ,[ex7_UpDown]
        ,[ex8_valAtRoot_Nothing, ex8_valAtRoot_Just]
        ,[ex9_treeSize]
        ,[ex10_leftest_Nothing, ex10_leftest_Just]
        ,[ex11_mapTree]
        ,[ex12_insertL]
        ,[ex13_measure]
        ,[property $ ex14_mysum, property $ ex14_mylength]
        ,[ex15_treeLeaves, ex15_treeSum]
        ,[ex16_rgb_red
         ,ex16_rgb_green
         ,ex16_rgb_blue
         ,ex16_rgb_darken
         ,ex16_rgb_mix
         ,ex16_rgb_complicated
         ]
        ]

-- -- -- -- -- -- -- --

ex1_safeDiv_zero x =
  safeDiv x 0 === Nothing

ex1_safeDiv_nonZero x y =
  (y/=0) ==> safeDiv x y === Just (div x y)

ex2_eitherDiv_zero x =
  eitherDiv x 0 === Left (show x++"/0")

ex2_eitherDiv_nonZero x y =
  (y/=0) ==> eitherDiv x y === Right (div x y)

ex3_mapMaybe_1 xs =
  counterexample ("let f True = Just True; f False = Nothing in mapMaybe f "++show xs) $
  mapMaybe f xs === filter id xs
  where f True = Just True
        f False = Nothing

ex3_mapMaybe_2 :: [Integer] -> Property
ex3_mapMaybe_2 is =
  counterexample ("let f x = if x>0 then Just (2*x) else Nothing\
                 \in mapMaybe f "++show is) $
  mapMaybe f is === map (2*) (filter (>0) is)
  where f x = if x>0 then Just (2*x) else Nothing

ex4_classify :: [Either Integer Bool] -> Property
ex4_classify es =
  classify es === partitionEithers es

ex5_fred = property $ do
  conjoin [counterexample "getName fred" $
           getName fred === "Fred"
          ,counterexample "getAge fred" $
           getAge fred === 90]

word = listOf1 (choose ('a','z'))

ex5_name = property $ do
  n <- word
  return $ counterexample ("getName (setName "++show n++" fred)") $
    getName (setName n fred) === n

ex5_age = property $ do
  a <- choose (0,89)
  return $ counterexample ("getAge (setAge "++show a++" fred)") $
    getAge (setAge a fred) === a

ex6_TwoCounters = property $ do
  a <- choose (0,20)
  b' <- choose (0,20)
  let b = a+b'
  let tc0 = iterate (incA . incB) zeros !! a
      tc1 = iterate incB tc0 !! b'
  return $ counterexample ("Did "++show a++" incAs and "++show b++" incBs.") $
    (getA tc1, getB tc1) === (a,b)

ex7_UpDown = property $ do
  a <- choose (0,20)
  b <- choose (0,20)
  let tc0 = iterate tick zero !! a
      tc1 = iterate tick (toggle tc0) !! b
  return $ counterexample ("Did "++show a++" ticks, a toggle, and "++show b++" ticks.") $
    get tc1 === a-b

ex8_valAtRoot_Nothing =
  valAtRoot Leaf === (Nothing :: Maybe Bool)

ex8_valAtRoot_Just = property $ do
  l <- genTree 3 :: Gen (Tree Integer)
  r <- genTree 3 :: Gen (Tree Integer)
  v <- choose (0,10 :: Integer)
  let t = Node v l r
  return $ counterexample (show t) $
    valAtRoot t === Just v

genTree :: Arbitrary a => Int -> Gen (Tree a)
genTree 0 = return Leaf
genTree siz = do
  let siz' = siz-1
  sizl <- choose (0,siz')
  let sizr = siz'-sizl
  l <- genTree sizl
  r <- genTree sizr
  v <- arbitrary
  return $ Node v l r

ex9_treeSize =
  forAllShrink (choose (0,50)) shrink $ \s -> do
    t <- genTree s
    return $ counterexample (show t) $
      treeSize (t :: Tree Int) === s

genLeft :: Arbitrary a => a -> Int -> Gen (Tree a)
genLeft k s = go s
  where go 0 = do t <- genTree s
                  return $ Node k Leaf t
        go n = do r <- genTree s
                  l <- go (n-1)
                  v <- arbitrary
                  return $ Node v l r

ex10_leftest_Nothing = leftest Leaf === (Nothing :: Maybe Bool)

ex10_leftest_Just = property $ do
  k <- choose (0,10)
  s <- choose (0,10)
  t <- genLeft k s
  return $ counterexample (show t) $
    leftest (t :: Tree Int) === Just k

genL :: Int -> Gen (Tree Bool -> Tree Bool)
genL s = go s
  where go 0 = do return $ id
        go n = do r <- genTree s
                  l <- go (n-1)
                  v <- arbitrary
                  return $ \k -> Node v (l k) r

ex11_mapTree =
  forAllShrink (choose (0,50)) shrink $ \s -> do
    t <- genTree s
    let t' = mapTree (even::Int->Bool) t
    return $ counterexample ("mapTree even "++show t++"\returned:\n"++show t') $
      check t t'
  where check Leaf Leaf = property $ True
        check (Node a al ar) bt@(Node b bl br) =
          counterexample ("Alipuussa "++show bt) $
          conjoin [b === even a,
                   check al bl,
                   check ar br]
        check a b =
          counterexample ("Tree structures don't match:\n"++show a++"\n"++show b) False

ex12_insertL =
  forAllShrink (choose (0,20)) shrink $ \s -> do
    f <- genL s
    let t0 = f Leaf
        t1 = f (Node True Leaf Leaf)
    return $ counterexample ("insertL True "++show t0) $
      insertL True t0 === t1

genMeasure 0 = return $ Leaf
genMeasure siz = do
  let siz' = siz-1
  sizl <- choose (0,siz')
  let sizr = siz'-sizl
  l <- genMeasure sizl
  r <- genMeasure sizr
  return $ Node siz l r

zeroTree Leaf = Leaf
zeroTree (Node _ l r) = Node 0 (zeroTree l) (zeroTree r)

ex13_measure =
  forAllShrink (choose (0,20)) shrink $ \s -> do
    t <- genMeasure s
    let t' = zeroTree t :: Tree Int
    return $ counterexample (show t') $
      measure t' === t

ex14_mysum :: [Int] -> Property
ex14_mysum xs =
  foldr sumf 0 xs === sum xs

ex14_mylength :: [Int] -> Property
ex14_mylength xs =
  foldr lengthf 0 xs === length xs

ex15_treeLeaves =
  forAllShrink (choose (0,20)) shrink $ \s -> do
    t <- genTree s
    let leaves = s+1
    return $ counterexample (show t) $
      foldTree leaft 1 (t :: Tree Bool) === leaves

modTree k Leaf = Leaf
modTree k (Node _ l r) = Node k (modTree k l) (modTree k r)

ex15_treeSum = property $ do
  k <- choose (0,5 :: Int)
  s <- choose (0,5)
  t0 <- genTree s :: Gen (Tree ())
  let t = modTree k t0
  return $ counterexample (show t) $
    foldTree sumt 0 t === s*k

ex16_rgb_red =
  counterexample (show Red) $
  rgb Red === [1,0,0]
ex16_rgb_green =
  counterexample (show Green) $
  rgb Green === [0,1,0]
ex16_rgb_blue =
  counterexample (show Blue) $
  rgb Blue === [0,0,1]

fcmp actual expected =
  counterexample ("Expected " ++ show expected ++ ", got " ++ show actual) $
    diff < eps
  where diff = sum . map abs $ zipWith (-) actual expected
        eps = 0.01

ex16_rgb_darken = property $ do
  s <- choose (0,1)
  let col = Darken s (Darken s Red)
  let ans = rgb col
  return $ counterexample (show col) $
    fcmp ans [(1-s)^2, 0, 0]

ex16_rgb_mix = property $ do
  r <- choose (0,1)
  g <- choose (0,1)
  let col = Mix (Darken r Red) (Darken g Green)
  let ans = rgb col
  return $ counterexample (show col) $
    fcmp ans [(1-r), (1-g), 0]

ex16_rgb_complicated = property $ do
  [r0,b0,g0,r1,b1,g1] <- replicateM 6 (choose (0,1))
  [x,y] <- replicateM 2 (choose (0,0.1))
  let c0 = Darken x (Mix (Darken (1-r0) Red) (Mix (Darken (1-b0) Blue) (Darken (1-g0) Green)))
      c1 = Mix (Darken (1-g1) Green) (Mix (Darken (1-b1) Blue) (Darken (1-r1) Red))
      c = Darken y (Mix c0 c1)
      ans = rgb c
      f = min 1
      x' = 1-x
      y' = 1-y
  return $ counterexample (show c) $
    fcmp ans [y'*(f $ x'*r0+r1),
              y'*(f $ x'*g0+g1),
              y'*(f $ x'*b0+b1)]
