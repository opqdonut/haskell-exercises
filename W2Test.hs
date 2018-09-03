module W2TestSansHT where

import Impl.Test
import W2
import Data.List
import Data.Char
import Test.QuickCheck hiding ((===))

main = testExs tests

tests = [[]
        ,[property ex2_measure_empty, property ex2_measure_nonEmpty]
        ,[ex3_takeFinal_1, ex3_takeFinal_2]
        ,[ex4_remove]
        ,[property ex5_substring]
        ,[property ex6_mymax]
        ,[property ex7_countSorted]
        ,[ex8_funny_1, ex8_funny_2]
        ,[property ex9_quicksort]
        ,[property ex10_powers]
        ,[property ex11_search_number, property ex11_search_string]
        ,[property ex12_fromTo]
        ,[property ex13_sums]
        ,[property ex14_mylast_nonempty, property ex14_mylast_empty]
        ,[property ex15_sorted_empty, property ex15_sorted_sorted]
        ,[property ex16_sumsOf]
        ,[property ex17_mymaximum_max, property ex17_mymaximum_min, property ex17_mymaximum_empty]
        ,[property ex18_map_1, property ex18_map_2]
        ,[property ex19_interpreter_1, property ex19_interpreter_2]
        ,[ex20_squares]]


-- -- -- -- -- --

ex2_measure_empty () = measure [] === -1
ex2_measure_nonEmpty (NonEmpty xs) = measure xs === length xs

ex3_takeFinal_1 =
  forAll (choose (0,20)) $ \n ->
  forAll (choose (0,n)) $ \k ->
  takeFinal k [0..n] === [n-k+1..n]

ex3_takeFinal_2 =
  forAll (choose (0,20)) $ \n ->
  forAll (choose (0,n)) $ \k ->
  takeFinal k (reverse [0..n]) === reverse [0..k-1]

ex4_remove =
  forAll (choose (0,20)) $ \n ->
  forAll (choose (0,n)) $ \k ->
  remove k [0,2..2*n] === map (2*) ([0..(k-1)] ++ [k+1..n])

ex5_substring = do
  base <- choose (ord 'a',ord 'f')
  len <- choose (0,20)
  let list = f [base..base+len-1]
  i <- choose (0,len)
  n <- choose (0,len-i)
  return $ counterexample ("substring "++show i++" "++show n++" "++show list) $
    substring i n list === f [base+i .. base + min (i+n) (len) - 1]
  where f = map chr

ex6_mymax = do
  t <- choose (0,20)
  f <- choose (0,20) `suchThat` \f -> f/=t
  let p True = t
      p False = f
  return $
    counterexample ("let p True = "++show t++"; p False = "++show f++" in mymax p False True") $
    mymax p False True === (t>f)

word = listOf1 (choose ('a','z'))
sortedWord = fmap sort word
unsortedWord = word `suchThat` \w -> w /= sort w

ex7_countSorted = do
  ss <- listOf1 sortedWord
  us <- listOf1 unsortedWord
  k <- choose (1,5)
  let ws = comb k ss us
  return $ counterexample ("countSorted "++show ws) $
    length ss === countSorted ws

  where comb k [] b = b
        comb k a b = take k a ++ comb k b (drop k a)

ex8_funny_1 =
  counterexample ("funny "++show inp) $
  funny inp === out
  where inp = ["a","bcdefgh","simo","xxxxxxxxxxx"]
        out = "BCDEFGH XXXXXXXXXXX"

ex8_funny_2 =
  counterexample ("funny "++show inp) $
  funny inp === out
  where inp = ["aaaaaa","bbbbbb","ccccc","ddddddd"]
        out = "AAAAAA BBBBBB DDDDDDD"

ex9_quicksort xs = quicksort xs === sort xs

ex10_powers = do
  n <- choose (2,5)
  len <- choose (1,10)
  end <- choose (n^(len-1),n^len-1)
  let p = powers n end
  return $ counterexample ("powers "++show n++" "++show end) $ conjoin
    [counterexample "all smaller than end" $
     all (<=end) p
    ,counterexample "sorted" $
     p == sort p
    ,counterexample "length" $
     length p === len
    ,counterexample "powers of n" $
     all (check n) p]
  where check n 0 = True
        check n 1 = True
        check n k
          | k `mod` n == 0    = check n (div k n)
          | otherwise         = False

ex11_search_number = do
  n <- choose (0,20 :: Integer)
  return $ counterexample ("search (+1) (=="++show n++") 0") $
    search (+1) (==n) 0 === n

ex11_search_string = do
  n <- word
  let w = n++n
      p = (==n)
  return $ counterexample ("search tail (=="++show n++") "++show w) $
    search tail p w == n



ex12_fromTo = do
  start <- choose (0,20)
  len <- choose (0,10)
  let end = start+len-1
  return $ counterexample ("fromTo "++show start++" "++show end) $
    fromTo start end === [start..end]

ex13_sums = do
  i <- choose (1,20)
  return $ counterexample ("sums "++show i) $
    sums i === scanl1 (+) [1..i]


ex14_mylast_nonempty :: NonEmptyList Integer -> Property
ex14_mylast_nonempty (NonEmpty xs) = mylast 0 xs === last xs
ex14_mylast_empty :: Char -> Property
ex14_mylast_empty i = mylast i [] === i

ex15_sorted_empty =
    counterexample "sorted []" $ sorted [] === True

ex15_sorted_sorted = do
  l <- vector 5
  let s = sort l
  return $ conjoin
    [counterexample ("sorted "++show l) $ sorted l === (s == l)
    ,counterexample ("sorted "++show s) $ sorted s === True]

ex16_sumsOf xs = sumsOf xs === scanl1 (+) xs

ex17_mymaximum_max :: NonEmptyList Integer -> Property
ex17_mymaximum_max (NonEmpty xs) = mymaximum compare 0 xs === maximum xs

ex17_mymaximum_min :: NonEmptyList Integer -> Property
ex17_mymaximum_min (NonEmpty xs) = mymaximum (\x y -> compare y x) 0 xs === minimum xs

ex17_mymaximum_empty = do
  i <- choose (True,False)
  return $ mymaximum compare i [] === i

ex18_map_1 = do
  i <- arbitrary :: Gen [Int]
  j <- arbitrary :: Gen [Bool]
  return $ counterexample ("map2 const "++show i++" "++show j) $
    map2 const i j === take (length j) i

ex18_map_2 = do
  i <- arbitrary :: Gen [Int]
  j <- arbitrary :: Gen [Int]
  return $ counterexample ("map2 (+) "++show i++" "++show j) $
    map2 (+) i j === zipWith (+) i j


ex19_interpreter_1 = do
  a0 <- choose (10,20)
  a1 <- choose (0,10)
  b0 <- choose (1,3)
  b1 <- choose (0,1)

  let first = replicate a0 "incA" ++ replicate b0 "incB" ++ ["printA","printB"]
      second = replicate (a0-a1) "decA" ++ replicate (b0-b1) "decB" ++ ["printB","printA"]
      input = first ++ second
      output = [show a0, show b0, show b1, show a1]

  return $ counterexample ("interpreter "++show input) $
    interpreter input === output

ex19_interpreter_2 = do
  nums <- vectorOf 4 $ choose (0,10)
  let diffs = zipWith (-) nums (0:nums)
      f x | x<0 = replicate (negate x) "decA"
          | otherwise = replicate x "incA"
      input = concatMap (\x -> f x ++ ["printA"]) diffs
      output = map show nums
  return $ counterexample ("interpreter "++show input) $
    interpreter input === output

ex20_squares =
  forAll (choose (0,1000)) $ \n ->
  let ret = squares n
  in conjoin [counterexample "length" $ length ret === n,
              counterexample "all squares" $ all isSq ret,
              counterexample "in order" $ sort ret == ret,
              counterexample "start and end" $ all check ret]

  where isSq x = x == (isqrt x)^2
        isqrt = round . sqrt . fromIntegral
        check x = let s = show x in head s == last s
