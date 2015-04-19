module W5Test where

import W5
import Impl.Test

import Data.List
import Data.Maybe
import System.Random

import Test.QuickCheck hiding (Result,reason,classify,Failure,(===))

main = testExs tests

x = property False

tests = [[property ex1_1, property ex1_2]
        ,[ex2]
        ,[ex3]
        ,[property ex4_eq, property ex4_neq, property ex4_len]
        ,[ex5]
        ,[ex6]
        ,[ex7]
        ,[property ex8_eq, property ex8_neq]
        ,[property ex9_bops, property ex9_sops, property ex9_fI]
        ,[property ex10_1, property ex10_2]
        ,[ex11_1, ex11_2]
        ,[property ex12_eq, property ex12_neq]
        ,[property ex13_list, property ex13_maybe]
        ,[property ex14_num, ex14_empties]
        ,[property ex15_num, property ex15_bool]
        ,[property ex16_1, property ex16_2]
        ,[ex17]
        ,[ex18]
        ]

-- -- -- -- -- -- -- --

ex1_1 a b = a %$ b === (a ++ b ++ a)

ex1_2 :: NonNegative Int -> Int -> Property
ex1_2 (NonNegative n) v =
  counterexample (show n ++ " *! " ++ show v) $
  conjoin [counterexample "length" $ length res === n
          ,counterexample "values" $ all (==v) res]
  where res = n *! v

m_t2 input exp = counterexample (show input) $ allEqual input === exp

g_t2 input = let x = input++[False,True] in m_t2 x False

ex2 = conjoin [m_t2 ([] :: [Bool]) True
                  ,m_t2 [True] True
                  ,m_t2 [0] True
                  ,m_t2 [0,0,0] True
                  ,m_t2 [0,0,1] False
                  ,property g_t2
                  ]

m_t3 input =
  length input > 2 ==>
    case secondSmallest input of
      Just s ->
        property $
         length (filter (<s) input) == 1
         || (s == minimum input && length (filter (==s) input) > 1)
      Nothing -> property $ counterexample "expected Just, was Nothing" False

ex3 = (m_t3 :: [Int] -> Property)
          .&. (m_t3 :: [Double] -> Property)
          .&. counterexample (show [1]) (secondSmallest ([1] :: [Int]) == Nothing)

ex4_eq :: [Integer] -> Property
ex4_eq xs =
  counterexample ("findDifference "++show xs++" "++show xs) $
    isNothing (findDifference xs xs)

ex4_neq (NonEmpty bs) = property $
  do
    i <- choose (0,length bs - 2)
    let (a,x:b) = splitAt i bs
        bs' = a ++ not x : b
     in return $ counterexample ("findDifference "++show bs++" "++show bs') $
      case findDifference bs bs' of
           Nothing -> counterexample "was Nothing, expected Just" False
           Just s -> s === (show x ++ " /= " ++ show (not x))

ex4_len :: [Char] -> [Char] -> Property
ex4_len s s' =
  l /= l' ==> counterexample ("findDifference "++show s++" "++show s') (findDifference s s' === Just err)
  where l = length s
        l' = length s'
        err = show l ++ " /= " ++ show l'

m_t5 input exp =
  counterexample (show input) $
    average input === exp

ex5 = m_t5 [1,2,3] 2
          .&. m_t5 [9,9,9,9] 9
          .&. m_t5 [1,2,3,4] 2.5
          .&. m_t5 (replicate 10 1 ++ replicate 10 2) 1.5

m_t6 x y exp =
  counterexample (show x ++ " == " ++ show y) $
   (x == y) === exp

ex6 = m_t6 Bar Bar True
          .&. m_t6 Quux Quux True
          .&. m_t6 Xyzzy Xyzzy True
          .&. m_t6 Bar Quux False
          .&. m_t6 Bar Xyzzy False
          .&. m_t6 Quux Bar False
          .&. m_t6 Quux Xyzzy False
          .&. m_t6 Xyzzy Bar False
          .&. m_t6 Xyzzy Quux False

ex7 = conjoin
  [Quux ?<= Quux, Quux ?<= Bar, Quux ?<= Xyzzy
  ,Bar ?<= Bar, Bar ?<= Xyzzy
  ,Xyzzy ?<= Xyzzy
  ,Bar ?> Quux
  ,Xyzzy ?> Bar, Xyzzy ?> Quux
  ,counterexample ("compare Bar Xyzzy") (compare Bar Xyzzy === LT)
  ,counterexample ("compare Quux Quux") (compare Quux Quux === EQ)
  ,counterexample ("Xyzzy > Quux") ((Xyzzy > Quux) === True)
  ,counterexample ("min Xyzzy Bar") (min Xyzzy Bar === Bar)
  ,counterexample ("max Bar Quux") (max Bar Quux === Bar)
  ,counterexample ("compare Xyzzy Xyzzy") (compare Xyzzy Xyzzy == EQ)
  ,counterexample ("compare Bar Bar") (compare Bar Bar == EQ)]
  where x ?<= y = counterexample (show x ++ " <= " ++ show y) ((x <= y) == True)
        x ?>  y = counterexample (show x ++ " > " ++ show y) ((x > y) == True)

ex8_eq a b c =
  let v = Vector a b c in
  counterexample (show v ++ " == " ++ show v) $
   (v == v) === True

ex8_neq a b c d e f =
  let v = Vector a b c
      v2 = Vector d e f
  in counterexample (show v ++ " == " ++ show v2) $
     (v == v2) === ((a,b,c)==(d,e,f))

ex9_bops a b c d e f =
  let v1 = Vector a b c
      v2 = Vector d e f
      g0 (Vector a _ _) = a
      g1 (Vector _ a _) = a
      g2 (Vector _ _ a) = a
  in conjoin
     [counterexample (show v1 ++ " + " ++ show v2) $
      conjoin [g0 (v1+v2) === a+d
              ,g1 (v1+v2) === b+e
              ,g2 (v1+v2) === c+f]
     ,counterexample (show v1 ++ " * " ++ show v2) $
      conjoin [g0 (v1*v2) === a*d
              ,g1 (v1*v2) === b*e
              ,g2 (v1*v2) === c*f]
     ,counterexample (show v1 ++ " - " ++ show v2) $
      conjoin [g0 (v1-v2) === a-d
              ,g1 (v1-v2) === b-e
              ,g2 (v1-v2) === c-f]
     ]

ex9_sops a b c =
  let v = Vector a b c
      g0 (Vector a _ _) = a
      g1 (Vector _ a _) = a
      g2 (Vector _ _ a) = a
  in conjoin
     [counterexample ("abs ("++show v++")") $
      conjoin [g0 (abs v) === abs a
              ,g1 (abs v) === abs b
              ,g2 (abs v) === abs c]
     ,counterexample ("signum ("++show v++")") $
      conjoin [g0 (signum v) === signum a
              ,g1 (signum v) === signum b
              ,g2 (signum v) === signum c]
     ,counterexample ("negate ("++show v++")") $
      conjoin [g0 (negate v) === negate a
              ,g1 (negate v) === negate b
              ,g2 (negate v) === negate c]]

ex9_fI a =
  fromIntegral a === Vector a a a

ex10_1 bs =
  let out = freqs bs
      (t,f) = partition id bs
  in (counterexample "number of True values" $
      null t || (length t,True) `elem` out)
     .&.
     (counterexample "number of False values" $
      null f || (length f,False) `elem` out)

ex10_2 :: [Integer] -> Property
ex10_2 is =
  let out = freqs is
      vals = nub is
  in (counterexample "return list length" $
      length out === length vals)
     .&&.
     (foldl (.&&.) (property True) $ map (ck out is) vals)
  where ck out vals i = let exp = length (filter (==i) vals)
                        in counterexample ("Does the result "++show out++" contain "++show(exp,i)) $ (exp,i) `elem` out

genTree :: Int -> Gen ITree
genTree 0 = return ILeaf
genTree siz = do
  let siz' = siz-1
  sizl <- choose (0,siz')
  let sizr = siz'-sizl
  l <- genTree sizl
  r <- genTree sizr
  v <- choose (0,10)
  return $ INode v l r

modTree :: ITree -> Gen ITree
modTree ILeaf = do
  s <- choose (1,5)
  t <- genTree s
  return $ t
modTree (INode x l r) =
  oneof [return ILeaf,
         do x' <- choose (0,10) `suchThat` (/=x)
            return $ INode x' l r,
         do l' <- modTree l
            return $ INode x l' r,
         do r' <- modTree r
            return $ INode x l r']

ex11_1 =
  forAllShrink (choose (0,20)) shrink $ \s ->
  do t <- genTree s
     return $ counterexample (show t ++ "\n  ==\n"++show t) $ (t==t) == True

ex11_2 =
  forAllShrink (choose (0,20)) shrink $ \s ->
  do t <- genTree s
     t2 <- modTree t
     return $ counterexample (show t ++ "\n  ==\n"++show t2) $ (t==t2) == False

ex12_eq :: [Bool] -> Property
ex12_eq xs =
  let l = foldr LNode Empty xs  in
  counterexample (show l ++ " == " ++ show l) $
  (l == l) === True

ex12_neq :: [Integer] -> [Integer] -> Property
ex12_neq xs ys =
  let l = foldr LNode Empty xs
      l2 = foldr LNode Empty ys
  in
   counterexample (show l ++ " == "++ show l2) $
  (l == l2) === (xs == ys)

ex13_list :: [Integer] -> Property
ex13_list xs = counterexample (show xs) $
  incrementAll xs === map (+1) xs

ex13_maybe :: Maybe Integer -> Property
ex13_maybe m = counterexample (show m) $
  incrementAll m === case m of Nothing -> Nothing
                               Just x -> Just (x+1)

ex14_num k =
  counterexample ("fmap (+1) (MkResult "++show k) $
  fmap (+(1::Int)) (MkResult k) === MkResult (k+1)

ex14_empties =
  (counterexample ("fmap not NoResult") $
   fmap not NoResult === NoResult)
  .&.
  (counterexample ("fmap not (Fail \"moi\")") $
   fmap not (Failure "moi") === Failure "moi")

ex15_num :: [Int] -> Property
ex15_num xs =
  let l = foldr LNode Empty xs in
  counterexample ("fmap (+1) "++show l) $
    ck (fmap (+1) l) (map (+1) xs)

ex15_bool bs =
  let l = foldr LNode Empty bs in
  counterexample ("fmap not "++show l) $
    ck (fmap not l) (map not bs)

ck :: (Eq a, Show a) => List a -> [a] -> Property
ck Empty [] = property True
ck (LNode x xs) (y:ys) = (x === y) .&&. ck xs ys
ck Empty ys = counterexample "Result list ended too soon!" False
ck xs [] = counterexample "Result list was too long!" False

ex16_1 i =
  counterexample ("runFun (fmap not (Fun even)) "++show i) $
    runFun (fmap not (Fun even)) i === odd i

ex16_2 i =
  counterexample ("runFun (fmap (*2) (Fun (\\i -> i))) "++show i) $
    runFun (fmap (*2) (Fun id)) i === 2*i

ex17 = property $
       do s <- choose (0,10)
          let g = mkStdGen s
              (a,b,c) = threeRandom g :: (Int,Int,Int)
          return $ counterexample ("values were not different: threeRandom (mkStdGen "++show s++")") $
            conjoin [a/=b,
                     a/=c,
                     b/=c]

shape :: (Show a, Show b) => Tree a -> Tree b -> Property
shape Leaf Leaf = property $ True
shape (Node _ l r) (Node _ l' r') =
  conjoin [shape l l',
           shape r r']
shape x y = counterexample ("Trees don't have the same shape:\n"++show x++"\n"++show y)
            False

genTree' :: Int -> Gen (Tree Bool)
genTree' 0 = return Leaf
genTree' siz = do
  let siz' = siz-1
  sizl <- choose (0,siz')
  let sizr = siz'-sizl
  l <- genTree' sizl
  r <- genTree' sizr
  v <- arbitrary
  return $ Node v l r

v Leaf = []
v (Node x l r) = x : v l ++ v r

ex18 = forAllShrink (choose (0,10)) shrink $ \siz ->
  do s <- choose (0,10)
     t <- genTree' siz
     let g = mkStdGen s
         (t',_) = randomizeTree t g :: (Tree Int,StdGen)
         vals = v t'
     return $ counterexample ("randomizeTree ("++show t++") (mkStdGen "++show s++")") $
       conjoin [shape t t'
               ,counterexample "values were not different" $ vals == nub vals]
