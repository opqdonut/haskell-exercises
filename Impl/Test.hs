module Impl.Test where

import Control.Monad
import Test.QuickCheck hiding (Result,reason)
import Test.QuickCheck.Property
import Test.QuickCheck.Test hiding (Result)
import Test.QuickCheck.Monadic

import GHC.IO.Handle
import System.Directory
import System.IO
import Control.Exception (finally)

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
  counterexample ("Expected " ++ show expected ++ ", got " ++ show actual) $ actual == expected

capture :: String -> IO a -> IO (String,a)
capture input op = do
  dir <- getTemporaryDirectory
  (path,h) <- openTempFile dir "jfo.in"
  hPutStrLn h input
  hClose h

  (opath,oh) <- openTempFile dir "jfo.out"

  mystdout <- hDuplicate stdout
  mystdin <- hDuplicate stdin

  read <- openFile path ReadMode
  hDuplicateTo read stdin
  hDuplicateTo oh stdout

  -- TODO catch
  val <- op `finally` do
    hDuplicateTo mystdin stdin
    hDuplicateTo mystdout stdout
    hClose oh

  str <- readFile opath

  return (str,val)

runc string op = run (capture string op)

runc' op = run (capture "" op)
