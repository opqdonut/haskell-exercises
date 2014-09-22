module W3Test where

import W3
import Impl.Test

import Data.List
import Data.Either
import Control.Monad

import Test.QuickCheck hiding (Result,reason,classify)
import Test.QuickCheck.Test
import Test.QuickCheck.Property hiding (classify)

main = testExs tests

tests = [[property $ prop_t1_safeDiv_zero, property $ prop_t1_safeDiv_nonZero]
        ,[property $ prop_t2_eitherDiv_zero
         ,property $ prop_t2_eitherDiv_nonZero]
        ,[property $ prop_t3_mapMaybe_1, property $ prop_t3_mapMaybe_2]
        ,[property $ prop_t4_classify]
        ,[prop_t5_matti, prop_t5_age, prop_t5_name]
        ,[prop_t6_TwoCounters]
        ,[prop_t7_UpDown]
        ,[prop_t8_valAtRoot_Nothing, prop_t8_valAtRoot_Just]
        ,[prop_t9_treeSize]
        ,[prop_t10_leftest_Nothing, prop_t10_leftest_Just]
        ,[prop_t11_mapTree]
        ,[prop_t12_insertL]
        ,[prop_t13_measure]
        ,[property $ prop_t14_mysum, property $ prop_t14_mylength]
        ,[prop_t15_treeLeaves, prop_t15_treeSum]
        ,[prop_t16_rgb_red
         ,prop_t16_rgb_green
         ,prop_t16_rgb_blue
         ,prop_t16_rgb_darken
         ,prop_t16_rgb_mix
         ,prop_t16_rgb_complicated
         ]
        ]

-- -- -- -- -- -- -- --

prop_t1_safeDiv_zero x =
  safeDiv x 0 === Nothing

prop_t1_safeDiv_nonZero x y =
  (y/=0) ==> safeDiv x y === Just (div x y)

prop_t2_eitherDiv_zero x =
  eitherDiv x 0 === Left (show x++"/0")

prop_t2_eitherDiv_nonZero x y =
  (y/=0) ==> eitherDiv x y === Right (div x y)

prop_t3_mapMaybe_1 xs =
  printTestCase ("let f True = Just True; f False = Nothing in mapMaybe f "++show xs) $
  mapMaybe f xs === filter id xs
  where f True = Just True
        f False = Nothing

prop_t3_mapMaybe_2 :: [Integer] -> Property
prop_t3_mapMaybe_2 is =
  printTestCase ("let f x = if x>0 then Just (2*x) else Nothing\
                 \in mapMaybe f "++show is) $
  mapMaybe f is === map (2*) (filter (>0) is)
  where f x = if x>0 then Just (2*x) else Nothing

prop_t4_classify :: [Either Integer Bool] -> Property
prop_t4_classify es =
  classify es === partitionEithers es

prop_t5_matti = do
  conjoin [printTestCase "getName matti" $
           getName matti === "Matti"
          ,printTestCase "getAge matti" $
           getAge matti === 90]

word = listOf1 (choose ('a','z'))

prop_t5_name = do
  n <- word
  printTestCase ("getName (setName "++show n++" matti)") $
    getName (setName n matti) === n

prop_t5_age = do
  a <- choose (0,89)
  printTestCase ("getAge (setAge "++show a++" matti)") $
    getAge (setAge a matti) === a

prop_t6_TwoCounters = do
  a <- choose (0,20)
  b' <- choose (0,20)
  let b = a+b'
  let tc0 = iterate (incA . incB) zeros !! a
      tc1 = iterate incB tc0 !! b'
  printTestCase ("Tehtiin "++show a++"kpl incA ja "++show b++"kpl incB.") $
    (getA tc1, getB tc1) === (a,b)

prop_t7_UpDown = do
  a <- choose (0,20)
  b <- choose (0,20)
  let tc0 = iterate tick zero !! a
      tc1 = iterate tick (toggle tc0) !! b
  printTestCase ("Tehtiin "++show a++"kpl tick, toggle, ja "++show b++"kpl tick.") $
    get tc1 === a-b

prop_t8_valAtRoot_Nothing =
  valAtRoot Leaf === (Nothing :: Maybe Bool)

prop_t8_valAtRoot_Just = do
  l <- genTree 3 :: Gen (Tree Integer)
  r <- genTree 3 :: Gen (Tree Integer)
  v <- choose (0,10 :: Integer)
  let t = Node v l r
  printTestCase (show t) $
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

prop_t9_treeSize =
  forAllShrink (choose (0,50)) shrink $ \s -> do
    t <- genTree s
    printTestCase (show t) $
      treeSize (t :: Tree Int) === s

genLeft :: Arbitrary a => a -> Int -> Gen (Tree a)
genLeft k s = go s
  where go 0 = do t <- genTree s
                  return $ Node k Leaf t
        go n = do r <- genTree s
                  l <- go (n-1)
                  v <- arbitrary
                  return $ Node v l r

prop_t10_leftest_Nothing = leftest Leaf === (Nothing :: Maybe Bool)

prop_t10_leftest_Just = do
  k <- choose (0,10)
  s <- choose (0,10)
  t <- genLeft k s
  printTestCase (show t) $
    leftest (t :: Tree Int) === Just k

genL :: Int -> Gen (Tree Bool -> Tree Bool)
genL s = go s
  where go 0 = do return $ id
        go n = do r <- genTree s
                  l <- go (n-1)
                  v <- arbitrary
                  return $ \k -> Node v (l k) r

prop_t11_mapTree =
  forAllShrink (choose (0,50)) shrink $ \s -> do
    t <- genTree s
    let t' = mapTree (even::Int->Bool) t
    printTestCase ("mapTree even "++show t++"\nPalautti:\n"++show t') $
      check t t'
  where check Leaf Leaf = property $ True
        check (Node a al ar) bt@(Node b bl br) =
          printTestCase ("Alipuussa "++show bt) $
          conjoin [b === even a,
                   check al bl,
                   check ar br]
        check a b =
          printTestCase ("Puitten rakenteet eivat tasmaa:\n"++show a++"\n"++show b) False

prop_t12_insertL =
  forAllShrink (choose (0,20)) shrink $ \s -> do
    f <- genL s
    let t0 = f Leaf
        t1 = f (Node True Leaf Leaf)
    printTestCase ("insertL True "++show t0) $
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

prop_t13_measure =
  forAllShrink (choose (0,20)) shrink $ \s -> do
    t <- genMeasure s
    let t' = zeroTree t :: Tree Int
    printTestCase (show t') $
      measure t' === t

prop_t14_mysum xs =
  foldr sumf 0 xs === sum xs

prop_t14_mylength :: [Int] -> Property
prop_t14_mylength xs =
  foldr lengthf 0 xs === length xs

prop_t15_treeLeaves =
  forAllShrink (choose (0,20)) shrink $ \s -> do
    t <- genTree s
    let leaves = s+1
    printTestCase (show t) $
      foldTree leaft 1 (t :: Tree Bool) === leaves

modTree k Leaf = Leaf
modTree k (Node _ l r) = Node k (modTree k l) (modTree k r)

prop_t15_treeSum = do
  k <- choose (0,5 :: Int)
  s <- choose (0,5)
  t0 <- genTree s :: Gen (Tree ())
  let t = modTree k t0
  printTestCase (show t) $
    foldTree sumt 0 t === s*k

prop_t16_rgb_red =
  printTestCase (show Red) $
  rgb Red === [1,0,0]
prop_t16_rgb_green =
  printTestCase (show Green) $
  rgb Green === [0,1,0]
prop_t16_rgb_blue =
  printTestCase (show Blue) $
  rgb Blue === [0,0,1]

fcmp actual expected =
  printTestCase ("Expected " ++ show expected ++ ", got " ++ show actual) $
    diff < eps
  where diff = sum . map abs $ zipWith (-) actual expected
        eps = 0.01

prop_t16_rgb_darken = do
  s <- choose (0,1)
  let col = Darken s (Darken s Red)
  let ans = rgb col
  printTestCase (show col) $
    fcmp ans [(1-s)^2, 0, 0]

prop_t16_rgb_mix = do
  r <- choose (0,1)
  g <- choose (0,1)
  let col = Mix (Darken r Red) (Darken g Green)
  let ans = rgb col
  printTestCase (show col) $
    fcmp ans [(1-r), (1-g), 0]

prop_t16_rgb_complicated = do
  [r0,b0,g0,r1,b1,g1] <- replicateM 6 (choose (0,1))
  [x,y] <- replicateM 2 (choose (0,0.1))
  let c0 = Darken x (Mix (Darken (1-r0) Red) (Mix (Darken (1-b0) Blue) (Darken (1-g0) Green)))
      c1 = Mix (Darken (1-g1) Green) (Mix (Darken (1-b1) Blue) (Darken (1-r1) Red))
      c = Darken y (Mix c0 c1)
      ans = rgb c
      f = min 1
      x' = 1-x
      y' = 1-y
  printTestCase (show c) $
    fcmp ans [y'*(f $ x'*r0+r1),
              y'*(f $ x'*g0+g1),
              y'*(f $ x'*b0+b1)]
