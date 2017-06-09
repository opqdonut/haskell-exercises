module Impl.Test where

import Control.Monad
import Test.QuickCheck hiding (Result,reason,(===))
import Test.QuickCheck.Test hiding (Result)
import Test.QuickCheck.Monadic

import GHC.IO.Handle
import System.Directory
import System.IO
import Control.Exception (bracket,finally)

-- toplevel

myArgs = stdArgs {maxSize = 100}

testEx str ts args = do
  putStrLn ("Testing "++str)
  res <- mapM (quickCheckWithResult args) ts
  if all isSuccess res
    then putStrLn "PASS" >> return True
    else putStrLn "FAIL" >> return False

testExsArgs tests args = do
  sucs <- forM (zip [1..] tests) $ \(i,ts) -> testEx (show i) ts args
  let success = length . filter id $ sucs
      total = length tests
  putStrLn $ "TOTAL: "++show success++" / "++show total

testExs tests = testExsArgs tests myArgs

-- utils

infixl 5 ===
actual === expected =
  counterexample ("Expected " ++ show expected ++ ", got " ++ show actual) $ actual == expected

counterexample' :: Testable prop => String -> prop -> Gen Property
counterexample' s p = return (counterexample s p)

-- monadic tests

withOverrideHandle :: Handle -> Handle -> IO a -> IO a
withOverrideHandle new old op =
  bracket (hDuplicate old) hClose $ \oldcopy ->
  bracket (hDuplicateTo new old) (\_ -> hDuplicateTo oldcopy old) $ \_ ->
  op

withStdinout :: Handle -> Handle -> IO a -> IO a
withStdinout newin newout =
  withOverrideHandle newin stdin . withOverrideHandle newout stdout

capture :: String -> IO a -> IO (String,a)
capture input op = do
  dir <- getTemporaryDirectory
  (path,h) <- openTempFile dir "haskell-exercises.in"
  hPutStrLn h input
  hClose h

  (opath,oh) <- openTempFile dir "haskell-exercises.out"
  read <- openFile path ReadMode

  val <- withStdinout read oh op `finally`
    do hClose oh
       hClose read

  str <- readFile opath

  return (str,val)

runc string op = run (capture string op)

runc' op = run (capture "" op)
