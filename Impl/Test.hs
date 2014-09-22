module Impl.Test where

import Control.Monad
import Test.QuickCheck hiding (Result,reason)
import Test.QuickCheck.Property
import Test.QuickCheck.Test hiding (Result)

args = stdArgs {maxSize = 100}

testEx str ts = do
  putStrLn ("Testing "++str)
  res <- mapM (quickCheckWithResult args) ts
  if all isSuccess res
    then putStrLn "PASS" >> return True
    else putStrLn "FAIL" >> return False

testExs tests = do
  sucs <- forM (zip [1..] tests) $ \(i,ts) -> testEx (show i) ts
  let success = length . filter id $ sucs
      total = length tests
  putStrLn $ "TOTAL: "++show success++" / "++show total

infixl 5 ===
actual === expected =
  printTestCase ("Expected " ++ show expected ++ ", got " ++ show actual) $ actual == expected
