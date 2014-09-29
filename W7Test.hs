module W7Test where

import W7

import Impl.Test
import Data.List
import Data.Ord
import Data.Function
import Control.Monad
import Control.Monad.State

import Test.QuickCheck hiding (Result,reason,classify,Failure,(===))

main = testExs tests -- $forAllProperties (quickCheckWithResult args)

tests = [[ex1_1, property ex1_2]
        ,[property ex2]
        ,[property ex3_1, property ex3_2]
        ,[property ex4_1, property ex4_2]
        ,[property ex5_getters, property ex5_addPoints]
        ,[property ex6_height, property ex6_size]
        ,[property ex7_eq, property ex7_neq, property ex7_sort]
        ,[property ex8_ok, property ex8_fail]
        ,[property ex9_maybe, property ex9_state]
        ,[property ex10_odds]
        ]

-- -- -- -- -- -- -- --

ex1_1 =
  conjoin
  [pyramid 1 === "*\n"
  ,pyramid 2 === " *\n***\n"
  ,pyramid 3 === "  *\n ***\n*****\n"]

ex1_2 = forAll (choose (1,25)) $ \n ->
  let ls = lines (pyramid n)
  in conjoin [counterexample "number of lines" $ length ls === n
             ,counterexample "lengths of lines" $
              map length ls === [n,n+1..2*n-1]
             ,counterexample "numbers of stars" $
              map (length . filter (=='*')) ls === [1,3..2*n-1]
             ,counterexample "all lines consist of spaces followed by stars" $
              all ck0 ls
             ]
  where
    ck0 (' ':xs) = ck0 xs
    ck0 ('*':xs) = ck1 xs
    ck0 _ = False
    ck1 ('*':xs) = ck1 xs
    ck1 [] = True
    ck1 _ = False

ex2 :: [Int] -> Property
ex2 xs = everySecond xs === f xs
  where f xs = [x | (i,x) <- zip [0..] xs, even i]

ex3_1 = do
  ls <- listOf1 arbitrary :: Gen [Int]
  i <- choose (0,length ls-1)
  let l = ls !! i
      (g,q) = wrap ls
  counterexample' ("wrap "++show ls) $
    conjoin [counterexample ("get "++show i) $
             g i === l
            ,counterexample ("query "++show l) $
             q l === True]

ex3_2 = do
  ls <- listOf1 arbitrary :: Gen [Int]
  l <- arbitrary `suchThat` \i -> not (elem i ls)
  let (_,q) = wrap ls
  counterexample' ("wrap "++show ls) $
    counterexample' ("query "++show l) $
      q l === False

ex4_1 = do
  starts <- listOf1 (choose (0,10))
  seqs <- forM starts $ \s -> liftM (scanl (+) s) (listOf1 (choose (11,20)))
  let l = concat seqs
      res = increasings l
  counterexample' ("increasings "++show l) $
    res == seqs

ex4_2 = do
  l <- listOf1 (choose (0,20))
  let res = increasings l
  return $ conjoin
    [counterexample ("concat (increasings "++show l++")") $
     concat res === l
    ,counterexample ("increasings "++show l++"\nall increasing") $
     all p res]
  where p (x:y:xs) = x<y && p (y:xs)
        p _ = True

ex5_getters = do
  n <- word
  Positive o <- arbitrary :: Gen (Positive Int)
  let num = show o
      s = newStudent n num
      d = "newStudent "++show n++" "++show num
  return $ conjoin [
    counterexample ("getName $ "++d) $ getName s === n,
    counterexample ("getNumber $ "++d) $ getNumber s === num,
    counterexample ("getPoints $ "++d) $ getPoints s === 0]

ex5_addPoints = do
  n <- word
  Positive o <- arbitrary :: Gen (Positive Int)
  let num = show o
      s = newStudent n num
      d = "newStudent "++show n++" "++show num
      f x = "addPoints "++show x++" $ "
  a <- choose (0,20)
  b <- choose (0,20)
  c <- choose (-10,-1)
  counterexample' (f a ++ f c ++ f b ++ d) $
    getPoints (addPoints a $ addPoints c $ addPoints b $ s) === a+b

genH 0 = return leaf
genH h = do
  b <- arbitrary
  if b then genH2 h else genH3 h

genH2 h = do
  i <- choose (0,h-1)
  t0 <- genH i
  t1 <- genH (h-1)
  b <- arbitrary
  return $ if b then node2 t0 t1 else node2 t1 t0

genH3 h = do
  i <- choose (0,h-1)
  t0 <- genH i
  j <- choose (0,h-1)
  t1 <- genH j
  t2 <- genH (h-1)
  b <- choose (0,2::Int)
  return $ case b of 0 -> node3 t0 t1 t2
                     1 -> node3 t0 t2 t1
                     _ -> node3 t2 t0 t1

ex6_height =
  forAllShrink (choose (0,10::Int)) shrink $ \h ->
  do
    t <- genH h
    counterexample' ("treeHeight ("++show t++")") $ treeHeight t === h

genS 0 = return leaf
genS s = do
  b <- arbitrary
  if b then genS2 s else genS3 s

genS2 s = do
  i <- choose (0,s-1)
  t0 <- genS i
  t1 <- genS (s-1-i)
  return $ node2 t0 t1

genS3 s = do
  i <- choose (0,s-1)
  t0 <- genS i
  j <- choose (0,s-1-i)
  t1 <- genS j
  t2 <- genS (s-1-i-j)
  return $ node3 t0 t1 t2

ex6_size =
  forAllShrink (choose (0,20::Int)) shrink $ \s ->
  do
    t <- genS s
    counterexample' ("treeSize ("++show t++")") $ treeSize t === s

genExpr = do
  i <- choose (0,4::Int)
  case i of 4 -> do (e1,v1) <- genExpr
                    (e2,v2) <- genExpr
                    return (Plus e1 e2,v1+v2)
            3 -> do (e1,v1) <- genExpr
                    (e2,v2) <- genExpr `suchThat` \(_,v2) -> v2 /= 0
                    return (Div e1 e2,v1 `div` v2)
            _ -> do c <- choose (-10,10)
                    return (Constant c, c)

genExpr' k = do
  i <- choose (0,4::Int)
  case i of 4 -> do j <- choose (0,k)
                    e1 <- genExpr' j
                    e2 <- genExpr' (k-j)
                    return $ Plus e1 e2
            3 -> do m <- choose (1,4)
                    fuzz <- choose (0,k `div` 2)
                    e1 <- genExpr' (k*m+fuzz)
                    e2 <- genExpr' m
                    return $ Div e1 e2
            _ -> return $ Constant k

genSucc = fmap fst genExpr
genZero = genExpr' 0

genFail = do
  i <- choose (0,3::Int)
  k <- choose (False,True)
  case i of 3 -> do e1 <- if k then genSucc else genFail
                    e2 <- if k then genFail else genSucc
                    return $ Div e1 e2
            2 -> do e1 <- genSucc
                    e2 <- genZero
                    return $ Div e1 e2
            _ -> do e1 <- if k then genSucc else genFail
                    e2 <- if k then genFail else genSucc
                    return $ Plus e1 e2

ex7_eq s = counterexample ("fromString "++show s++" == fromString "++show s) $
               (fromString s == fromString s) === True

ex7_neq s s' = s /= s' ==> counterexample ("fromString "++show s++" == fromString "++show s')
                   ((fromString s == fromString s') === False)

word = listOf1 (choose ('a','z'))

ex7_sort = do
  words <- listOf1 word
  let res = map toString . sort $ map fromString words
      should = concat . map sort . groupBy ((==)`on`length) . sortBy (comparing length) $ words
  counterexample' ("map toString (sort (map fromString "++show words++"))") $
    res === should

ex8_ok = do
  (e,v) <- genExpr
  counterexample' ("safeEval ("++show e++")") $
    safeEval e === Just v

ex8_fail = do
  e <- genFail
  counterexample' ("safeEval ("++show e++")") $
    safeEval e === Nothing

t1 :: Int -> Int -> Maybe Bool
t1 k x = Just (x>k)

t2 :: Int -> Int -> State [Int] Bool
t2 k x = do modify (k:)
            return (x>k)

ex9_maybe = do
  ts <- listOf1 (choose (0,10))
  i <- choose (maximum ts+1,maximum ts+10)
  j <- choose (minimum ts,maximum ts)
  let ms = map t1 ts
      s = "["++intercalate "," (map f ts)++"]"
      f t = "test1 "++show t
      d = "test "++s++" "
  return $ conjoin [counterexample (d++show i) (test ms i === Just True)
                   ,counterexample (d++show j) (test ms j === Just False)]

ex9_state = do
  ts <- listOf1 (choose (0,10))
  i <- choose (maximum ts+1,maximum ts+10)
  j <- choose (minimum ts,maximum ts)
  let ms = map t2 ts
      s = "["++intercalate "," (map f ts)++"]"
      f t = "test2 "++show t
      d i = "runState (test "++s++" "++show i++") []"
      ts' = take (length (takeWhile (<j) ts) + 1) ts
  return $ conjoin
    [counterexample (d i) (runState (test ms i) [] === (True,reverse ts))
    ,counterexample (d j) (runState (test ms j) [] === (False,reverse ts'))]


ex10_odds = do
  n <- listOf (choose (0::Int,10))
  let res = map head . filter (odd.length) . group $ sort n
  counterexample' ("snd $ runState (odds "++show n++") []") $
    sort (snd (runState (odds n) [])) === res
