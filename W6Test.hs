module W6Test where

import W6

import Data.List
import Data.Char
import Data.Either
import Data.Ord
import Control.Monad.Trans.State

import Impl.Test
import Test.QuickCheck hiding (Result,reason,classify,Failure,(===))

main = testExs tests

tests :: [[Property]]
tests = [[property ex1_ok, property ex1_fail]
        ,[property ex2_ok, property ex2_fail]
        ,[property ex3_ok, property ex3_fail]
        ,[property ex4_1, property ex4_2, property ex4_3]
        ,[property ex5]
        ,[property ex6]
        ,[property ex7]
        ,[property ex8]
        ,[property ex9_Maybe, property ex9_State]
        ,[property ex10]
        ,[property ex11_dfs_1, property ex11_dfs_2
         ,property ex11_routeExists_basic, property ex11_routeExists]
        ,[property ex12_1, property ex12_2]
        ,[property ex13]
        ,[property ex14_sumBounded_ok, property ex14_sumBounded_fail
         ,property ex14_sumNotTwice]
        ,[property ex15]
        ,[property ex16_1, property ex16_2, property ex16_stress]
        ]

-- -- -- -- -- -- -- --

word = do fst <- choose ('A','Z')
          rest <- listOf (choose ('a','z'))
          return $ fst:rest

bad = do a <- choose ('A','Z')
         b <- word
         c <- elements "0123456789"
         d <- word
         return $ a:b++c:d

ex1_ok = do
  for <- word
  sur <- word
  let str = for++" "++sur
  counterexample' ("readNames "++show str) $
    readNames str === Just (for,sur)

m_ex1_fail s =
  counterexample ("readNames "++show s) $ readNames s === Nothing

ex1_fail =
  do for <- word
     sur <- word
     b <- bad
     return $ conjoin [m_ex1_fail (for ++ sur),
                       m_ex1_fail (map toLower for ++ " " ++ sur),
                       m_ex1_fail (for ++ " " ++ map toLower sur),
                       m_ex1_fail (for ++ b ++ " " ++ sur),
                       m_ex1_fail (for ++ " " ++ sur ++ b)]

ex2_ok = do
  as <- listOf1 arbitrary :: Gen [Int]
  i <- choose (0,length as)
  let ml = Just as
      mi = Just i
  counterexample' ("myTake ("++show mi++") ("++show ml++")") $
    myTake mi ml === Just (take i as)

ex2_fail = do
  as <- listOf1 arbitrary :: Gen [Int]
  i <- choose (length as+1,length as+5)
  let ml = Just as
      mi = Just i
  return $ conjoin [counterexample' ("myTake ("++show mi++") ("++show ml++")") $
                    myTake mi ml === Nothing,
                    counterexample' ("myTake Nothing ("++show ml++")") $
                    myTake Nothing ml === Nothing,
                    counterexample' ("myTake ("++show mi++") Nothing") $
                    myTake mi (Nothing :: Maybe String) === Nothing]

ex3_ok = do
  as <- listOf1 arbitrary :: Gen [Integer]
  is <- listOf (choose (0,length as - 1))
  counterexample' ("selectSum "++show as++" "++show is) $
    selectSum as is === Just (sum $ map (as!!) is)

ex3_fail = do
  as <- arbitrary :: Gen [Int]
  is1 <- listOf (choose (0,length as - 1))
  is2 <- listOf (choose (0,length as - 1))
  b <- elements [-1,length as]
  let is = is1++b:is2
  counterexample' ("selectSum "++show as++" "++show is) $
    selectSum as is === Nothing

b n k = case (n,k) of (_,0) -> 1
                      (0,_) -> 0
                      (n,k) -> b (n-1) (k-1) + b (n-1) k

ex4_1 = do
  n <- choose (0,7)
  k <- choose (0,n)
  let Logger _ res = binom n k
  counterexample' ("Return value of binom "++show n++" "++show k) $
    res === b n k

ex4_2 = do
  n <- choose (0,7)
  k <- choose (0,n)
  let Logger log _ = binom n k
  counterexample' ("Log of binom "++show n++" "++show k) $
    conjoin [counterexample' "log should not be empty" $
             not $ null log,
             counterexample' "last message of log" $
             last log === ("B("++show n++","++show k++")"),
             counterexample' "first message of log" $
             head log === ("B("++show (n-k)++",0)")]



ex4_3 =
  conjoin [t 2 2 ["B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)"],
           t 2 7 ["B(0,5)","B(0,6)","B(1,6)","B(0,6)","B(0,7)","B(1,7)","B(2,7)"],
           t 3 3 ["B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)","B(0,1)","B(0,2)","B(1,2)","B(0,2)","B(0,3)","B(1,3)","B(2,3)","B(3,3)"],
           t 4 3 ["B(1,0)","B(0,0)","B(0,1)","B(1,1)","B(2,1)","B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)","B(3,2)","B(0,0)","B(0,1)","B(1,1)","B(0,1)","B(0,2)","B(1,2)","B(2,2)","B(0,1)","B(0,2)","B(1,2)","B(0,2)","B(0,3)","B(1,3)","B(2,3)","B(3,3)","B(4,3)"]]
  where t n k log = counterexample' ("binom "++show n++" "++show k) $ let Logger l _ = binom n k in l===log

ex5 i = counterexample' ("runState update "++show i) $
        runState update i === ((),2*i+1)

ex6 bs = counterexample' ("runState (lengthAndCount True "++show bs++") 0") $
         runState (lengthAndCount True bs) 0 === (length bs, length (filter id bs))

ex7 = do
  is <- fmap nub $ listOf1 (choose ('a','z') :: Gen Char)
  fs <- vectorOf (length is) (choose (1,2048))
  let assocs = zip is fs
  x <- elements is
  y <- choose  ('0','z') `suchThat` \y -> not (elem y is)
  let Just cx = lookup x assocs
      ((),rx) = runState (count x) assocs
      ((),ry) = runState (count y) assocs
      s x = "runState (count "++show x++") "++show assocs
  return $ conjoin [counterexample' (s y) $
                    sort ry === sort ((y,1):assocs),
                    counterexample' (s x) $
                    sort rx === sort ((x,cx+1):delete (x,cx) assocs)]

ex8 =
  forAllShrink (listOf (choose (0,10 :: Integer))) shrink $ \is ->
  let (r,_) = runState (occurrences is) []
      ck i = ascs [r !! j | (j,x) <- zip [0..] is, x==i]
      ascs xs = xs == [1..length xs]
  in all ck (nub is)

ex9_Maybe :: Maybe Bool -> Maybe Int -> Maybe Int -> Property
ex9_Maybe b t e = ifM b t e === case b of Just True -> t
                                          Just False -> e
                                          Nothing -> Nothing

ex9_State = do
  b <- arbitrary
  t <- choose (1,1024 :: Int)
  e <- choose (1,1024 :: Int)
  counterexample' ("runState (ifM (return "++show b++") (modify (+ "++show t++")) (modify (+ "++show e++"))) 0") $
    runState (ifM (return b) (modify (+t)) (modify (+e))) 0 === ((), if b then t else e)

ex10 :: [Int] -> [Int] -> Property
ex10 as bs =
  counterexample ("mapM2 (\\x y -> if x == y then Nothing else Just (x-y)) "++show as++" "++show bs) $
    mapM2 (\x y -> if x == y then Nothing else Just (x-y)) as bs === res
  where z = zipWith (-) as bs
        res = if all (/=0) z then Just z else Nothing

ex11_dfs_1 = do
  let cs = [[1],[0,2],[1,3],[2,4],[3,5],[4]]
  i <- choose (1,length cs - 1)
  let st = [0..i-1]
  counterexample' ("runState (dfs "++show cs++" "++show i++") "++show st) $
    let ((),res) = runState (dfs cs i) st
    in sort res === [0..5]

ex11_dfs_2 = do
  let cs = [[1,4],[0,2],[1,3],[2,4],[3,0]]
  i <- choose (1,length cs - 1)
  counterexample' ("runState (dfs "++show cs++" "++show i++") []") $
    let ((),res) = runState (dfs cs i) []
    in sort res === [0..4]

ex11_routeExists_basic = do
  siz <- choose (2,5)
  let cs = map (\i -> delete i [0..siz-1]) [0..siz-1]
  a <- choose (0,siz-1)
  b <- choose (0,siz-1)
  counterexample' ("routeExists "++show cs++" "++show a++" "++show b) $
    routeExists cs a b === True

shuffle xs = do
  is <- vector (length xs) :: Gen [Int]
  return $ map snd . sortBy (comparing fst) $ zip is xs


genGraph' :: [Int] -> [Int] -> [(Int,Int)] -> Gen [(Int,Int)]
genGraph' is [] es = return es
genGraph' is todo es = do
  u <- elements $ todo
  v <- elements $ is \\ todo
  genGraph' is (delete u todo) ((u,v):(v,u):es)

genGraph :: [Int] -> Gen [(Int,Int)]
genGraph is = do
  base <- genGraph' is (tail is) []
  [a,b,c] <- vectorOf 3 (elements is)
  return $ (a,b):(b,c):base

mkGraph es = map neighs [0..n]
  where n = maximum (map fst es ++ map snd es)
        neighs i = nub $ sort $ map snd $ filter (\(x,_) -> x==i) es

ex11_routeExists = do
  siz <- choose (5,7)
  k <- choose (2,siz-2)
  left <- genGraph [0..k]
  right <- genGraph [k+1..siz-1]
  i <- choose (0,siz-1)
  j <- choose (0,siz-1)
  let cities = mkGraph (left++right)
  counterexample' (show left++"\n"++show right++"\n"++"routeExists "++show cities++" "++show i++" "++show j) $
    routeExists cities i j === ((i<=k) == (j<=k))

m is = maximum (scanl1 (+) is)

ex12_1 = do
  let n = 6
  is <- vectorOf n (choose (0,10))
  i <- choose (0,n-2)
  j <- choose (i+1,n-1)
  let a = is!!i
      b = is!!j
      ret = orderedPairs is
  counterexample' ("orderedPairs "++show is) $
    if a<b
    then counterexample' ("The pair "++show (a,b)++" should be in the list.") $ (a,b) `elem` ret
    else counterexample' ("The pair "++show (a,b)++" should not be in the list.") . not $ (a,b) `elem` ret

ex12_2 = do
  let n = 7
  let is0 = [0..n]
  x <- choose (0,n)
  let is = drop x is0 ++ take x is0
      exp = [(i,j) | i<-[0..x-2], j<-[i+1..x-1]]
            ++
            [(i,j) | i<-[x..n-1], j<-[i+1..n]]
  counterexample' ("orderedPairs "++show is) $
    sort (orderedPairs is) === sort exp

sums' [] = [0]
sums' (x:xs) = sums' xs ++ map (x+) (sums' xs)

ex13 = do
  siz <- choose (0,5)
  is <- vectorOf siz (choose (0,10))
  counterexample' ("sums "++show is) $
    nub (sort (sums is)) === nub (sort (sums' is))

ex14_sumBounded_ok =
  forAllShrink (listOf1 (choose (-10,10))) shrink $ \is ->
  let k = m is + 1
  in counterexample' ("sumBounded "++show k++" "++show is) $
     sumBounded k is === Just (sum is)

ex14_sumBounded_fail =
  forAll (listOf1 (choose (-10,10))) $ \is ->
  let k = m is - 1
  in counterexample' ("sumBounded "++show k++" "++show is) $
     sumBounded k is === Nothing

ex14_sumNotTwice is =
  sumNotTwice is === sum (nub is)

ex15 =
  let op :: Int -> Result Int
      op i = if i>3 then fail "big" else return (i+1)
      s = "let op i = if (i>3) then fail \"big\" else return (i+1) in "
  in conjoin [counterexample' (s++" MkResult 1 >>= op") $
              (MkResult 1 >>= op) === MkResult 2,
              counterexample' (s++" MkResult 4 >>= op") $
              (MkResult 4 >>= op) === Failure "big",
              counterexample' (s++" Fail \"foo\" >>= op") $
              (Failure "foo" >>= op) === Failure "foo",
              counterexample' (s++" NoResult >>= op") $
              (NoResult >>= op) === NoResult]

ex16_fmap_1 =
  do i <- choose (0,10)
     let op = fmap (+1) getSL
     counterexample' ("runSL (fmap (+1) getSL) " ++ show i) $
       runSL op i === (i,i+1,[])

ex16_fmap_2 =
  do m <- word
     s <- choose (0,10)
     let op = fmap (const True) (msgSL m)
     counterexample' ("runSL (fmap (const True) (msgSL "++show m++")) "++show s) $
       runSL op s === (True,s,[m])

ex16_1 =
  do i <- choose (0,10)
     let op = putSL i >> getSL >>= \i -> msgSL (show i)
         s = "putSL "++show i++" >> getSL >>= \\i -> msgSL (show i)"
     counterexample' ("runSL ("++s++") 1") $ runSL op 1 === ((),i,[show i])

ex16_2 =
  do msg <- word
     msg2 <- word
     i <- choose (0,10)
     j <- choose (0,10)
     let op = do msgSL msg
                 x <- getSL
                 msgSL (msg2++show x)
                 putSL (x+i)
                 return x
         s = "op = \ndo msgSL "++show msg++"\n   x <- getSL\n   msgSL ("++show msg2++"++show x)\n   putSL (x+"++show i++")\n   return x"
     counterexample' (s++"\nrunSL op "++show j) $ runSL op j === (j,j+i,[msg,msg2++show j])

ex16_stress =
  arbitrary >>= \o ->
  return . shrinking shrink o $ \ops ->
  let m (Left i) = modifySL (+i)
      m (Right s) = msgSL s
      s (Left i) = "modifySL (+"++show i++")"
      s (Right m) = "msgSL "++show m
      op = mapM_ m ops
      desc = "runSL ("++intercalate " >> " (map s ops)++") 0"
      (incs,msgs) = partitionEithers ops
      state = sum incs
  in counterexample' desc $ runSL op 0 === ((),state,msgs)
