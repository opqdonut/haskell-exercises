module W1Test where

import Impl.Test
import W1
import Data.List
import Test.QuickCheck hiding ((===))

main = testExs tests

tests = [[]
        ,[property ex2_double]
        ,[property ex3_quadruple]
        ,[property ex4_poly2]
        ,[property ex5_eeny_even, property ex5_meeny_odd]
        ,[property ex6_fizzbuzz_3_5
         ,property ex6_fizzbuzz_3
         ,property ex6_fizzbuzz_5
         ,property ex6_fizzbuzz_empty]
        ,[property ex7_isZero_0
         ,property ex7_isZero_positive
         ,property ex7_isZero_negative]
        ,[ex8_sumTo]
        ,[ex9_power]
        ,[property ex10_ilog2]
        ,[ex11_binomial]
        ,[ex12_tribonacci]
        ,[ex13_myGcd]
        ,[ex14_funnyCompare_even, ex14_funnyCompare_odd
         ,ex14_funnyCompare_mixed]
        ,[ex15_funnyMin_even, ex15_funnyMin_odd
         ,ex15_funnyMin_mixed]
        ,[ex16_pyramid]
        ,[ex17_smallestDivisor_prime, property ex17_smallestDivisor_comp]
        ,[ex18_isPrime]
        ,[ex19_nextPrime]]

-- -- -- -- -- --

ex2_double x = double x `div` 2 === x

ex3_quadruple x = quadruple x `div` 4 === x

feq a b = abs (a-b) < 0.01

ex4_poly2 = do
  x0 <- elements [1..4] :: Gen Double
  x1 <- elements [1..4] :: Gen Double
  a <- elements [1..4] :: Gen Double
  let b = -a*(x0+x1)
      c = a*x0*x1
      str = concat ["poly2 ",show a," ",show b," ",show c," "]
      t :: Double -> Double -> Property
      t x y = counterexample (str++show x++" should be "++show y) $ poly2 a b c x `feq` y
  return $ t x0 0
           .&&. t x1 0
           .&&. t ((x0+x1)/2) (-b^2/(4*a)+c)

ex5_eeny_even x = eeny (2*x) === "eeny"
ex5_meeny_odd x = eeny (2*x+1) === "meeny"

div35 :: Gen Integer
div35 = fmap (*15) arbitrary

div5 :: Gen Integer
div5 = fmap (\x -> 5*(3*x+1)) arbitrary

div3 :: Gen Integer
div3 = fmap (\x -> 3*(5*x+1)) arbitrary

div0 :: Gen Integer
div0 = fmap (\x -> 15*x+1) arbitrary

ex6_fizzbuzz_3_5 =
  forAll div35 $ \i ->
  fizzbuzz i === "FizzBuzz"
ex6_fizzbuzz_3 =
  forAll div3 $ \i ->
  fizzbuzz i === "Fizz"
ex6_fizzbuzz_5 =
  forAll div5 $ \i ->
  fizzbuzz i === "Buzz"
ex6_fizzbuzz_empty =
  forAll div0 $ \i ->
  fizzbuzz i === ""

ex7_isZero_0 = isZero 0 === True

ex7_isZero_positive :: Positive Integer -> Property
ex7_isZero_positive (Positive n) = isZero n === False

ex7_isZero_negative :: Positive Integer -> Property
ex7_isZero_negative (Positive n) = isZero (-n) === False

ex8_sumTo =
  forAll (elements [1..100]) $ \n ->
  sumTo n === sum [1..n]

ex9_power =
  forAll (elements [1..10]) $ \n ->
  forAll (elements [1..10]) $ \k ->
  power n k === n^k

ex10_ilog2 (Positive n) =
  ilog2 n === floor (logBase 2 $ fromIntegral n)

ex11_binomial =
  forAll (elements [0..10]) $ \n ->
  forAll (elements [0..n]) $ \k ->
  binomial n k === f n `div` (f k * f (n-k))
  where f n = product [1..n]

ex12_tribonacci =
  forAll (elements [1..15]) $ \n ->
  tribonacci n === t n
  where t 1 = 1
        t 2 = 1
        t 3 = 2
        t n = t (n-1) + t (n-2) + t (n-3)

ex13_myGcd =
  forAll (elements [1..max]) $ \x ->
  forAll (elements [1..max]) $ \y ->
  myGcd x y === gcd x y
  where max = 10000

odds = filter odd [-5..100]
evens = filter even [-5..100]

ex14_funnyCompare_even =
  forAll (elements evens) $ \x ->
  forAll (elements evens) $ \y ->
  funnyCompare x y === compare x y

ex14_funnyCompare_odd =
  forAll (elements odds) $ \x ->
  forAll (elements odds) $ \y ->
  funnyCompare x y === compare x y

ex14_funnyCompare_mixed =
  forAll (elements evens) $ \x ->
  forAll (elements odds) $ \y ->
  funnyCompare x y === LT
  .&&.
  funnyCompare y x === GT

ex15_funnyMin_even =
  forAll (elements evens) $ \x ->
  forAll (elements evens) $ \y ->
  funnyMin x y === min x y

ex15_funnyMin_odd =
  forAll (elements odds) $ \x ->
  forAll (elements odds) $ \y ->
  funnyMin x y === min x y

ex15_funnyMin_mixed =
  forAll (elements evens) $ \x ->
  forAll (elements odds) $ \y ->
  funnyMin x y === x
  .&&.
  funnyMin y x === x

split delim xs =
  case rest of [] -> [a]
               (_:rest') -> a : split delim rest'
  where (a,rest) = break (==delim) xs

ex16_pyramid =
  forAll (elements [0..40]) $ \n ->
  f (pyramid n) === ([0..n] ++ [n-1,n-2..0])
  where f xs = map read $ split ',' xs

primes = go [2..]
  where go (x:xs) = x : go (filter (notDivBy x) xs)
        notDivBy x y = mod y x /= 0

ex17_smallestDivisor_prime = do
  forAll (elements $ take 12 primes) $ \p ->
    p === smallestDivisor p

ex17_smallestDivisor_comp = do
  k <- (elements . take 10 $ primes)
  p <- (elements . take 20 . drop 10 $ primes)
  let n = k*p
  return $ counterexample (show n) $
           k === smallestDivisor n

ex18_isPrime =
  forAll (elements [0..max]) $ \n ->
  isPrime n === elem n primes'
  where max = 20
        primes' = takeWhile (<=max) primes

ex19_nextPrime =
  forAll (elements [0..max]) $ \n ->
  nextPrime n === head (dropWhile (<=n) primes)
  where max = 100
