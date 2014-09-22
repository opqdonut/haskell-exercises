module W4Test where

import W4
import Impl.Test

import Control.Monad
import Data.List
import Data.IORef
import System.IO
import System.Directory

import Test.QuickCheck hiding (Result,reason)
import Test.QuickCheck.Test
import Test.QuickCheck.Property
import Test.QuickCheck.All
import Test.QuickCheck.Monadic

main = testExs tests

tests = [[prop_t1_hello]
        ,[prop_t2_greet]
        ,[prop_t3_greet2]
        ,[prop_t4_readWords]
        ,[prop_t5_readUntil]
        ,[prop_t6_printFibs]
        ,[prop_t7_isums]
        ,[prop_t8_whenM_True, prop_t8_whenM_False]
        ,[prop_t9_while]
        ,[prop_t10_debug]
        ,[prop_t11_mapM_]
        ,[prop_t12_forM]
        ,[prop_t13_tuplaKutsu]
        ,[prop_t14_compose]
        ,[prop_t15_mkCounter]
        ,[prop_t16_hFetchLines]
        ,[prop_t17_readCSV]
        ,[prop_t18_compareFiles]
        ,[prop_t19_interact_terminates, prop_t19_interact_loop]
        ]

-- -- -- -- --

prop_t1_hello = monadicIO $ do
  (text,()) <- runc' hello
  stop $ text === "HELLO\nWORLD\n"

word = listOf1 (choose ('a','z'))

prop_t2_greet = monadicIO $ do
  name <- pick word
  (text,()) <- runc' $ greet name
  stop $ text === ("HELLO "++name++"\n")

prop_t3_greet2 =  monadicIO $ do
  name <- pick word
  (text,()) <- runc (name++"\n") greet2
  stop $ text === ("HELLO "++name++"\n")

prop_t4_readWords = monadicIO $ do
  words <- pick $ listOf1 word
  (_,ret) <- runc (unlines words) (readWords (length words - 1))
  stop $ ret === sort (init words)

prop_t5_readUntil = monadicIO $ do
  end <- pick word
  words <- pick $ listOf1 (word `suchThat` (/=end))
  let input = unlines $ words ++ [end]
  (_,ret) <- runc input (readUntil (==end))
  stop . printTestCase ("readUntil (==" ++ show end ++ ")\nInput: "++show input) $
    ret === words

prop_t6_printFibs = monadicIO $ do
  n <- pick $ choose (0,40)
  (text,_) <- runc' $ printFibs n
  stop . printTestCase ("printFibs "++show n) $
    text === unlines (map show (take n fibs))
  where fibs = 1:1:zipWith (+) fibs (tail fibs)

prop_t7_isums = monadicIO $ do
  numbers <- pick . listOf1 $ choose (-10,10)
  let n = length numbers
  (text,ret) <- runc (unlines $ map show numbers) $ isums n
  stop . printTestCase ("isums "++show n) $
    conjoin [printTestCase "returning" $
             ret === sum numbers,
             printTestCase "printing" $
             text === unlines (map show $ scanl1 (+) numbers)]

prop_t8_whenM_True = monadicIO $ do
  r <- run $ newIORef False
  let op = writeIORef r True
  let cond = return True
  run $ whenM cond op
  v <- run $ readIORef r
  stop $ printTestCase "whenM (return True)" $
    v

prop_t8_whenM_False = monadicIO $ do
  r <- run $ newIORef False
  let op = writeIORef r True
  let cond = return False
  run $ whenM cond op
  v <- run $ readIORef r
  stop $ printTestCase "whenM (return False)" $
    not v

prop_t9_while = monadicIO $ do
  i <- pick $ choose (0,10 :: Int)
  a <- run $ newIORef 0
  b <- run $ newIORef 0
  let ehto = modifyIORef a (+1) >> fmap (<=i) (readIORef a)
      op = modifyIORef b (+1)
  run $ while ehto op
  af <- run $ readIORef a
  bf <- run $ readIORef b
  stop $ printTestCase "while" $
    conjoin [printTestCase "number of calls to condition" $ af === i+1,
             printTestCase "number of calls to operation" $ bf === i]

prop_t10_debug = monadicIO $ do
  token <- pick word
  value <- pick word
  print <- pick word
  (text,ret) <- runc' $ debug token (putStrLn print >> return value)
  stop $ printTestCase ("debug "++show token++" (do putStrLn "++show print++"; return "++show value++")") $
    conjoin [printTestCase "tulostus" $ text === (token ++ "\n" ++ print ++ "\n" ++ token ++ "\n"),
             printTestCase "palautus" $ ret === value]

prop_t11_mapM_ = monadicIO $ do
  r <- run $ (newIORef [] :: IO (IORef [Int]))
  lis <- pick $ listOf1 arbitrary
  let op x = modifyIORef r (x:)
  run $ mymapM_ op lis
  ret <- run $ readIORef r
  stop $ printTestCase ("mapM op "++show lis) $
    ret === reverse lis

prop_t12_forM = monadicIO $ do
  r <- run $ (newIORef [] :: IO (IORef [Int]))
  lis <- pick $ listOf1 arbitrary
  let op x = do modifyIORef r (x:)
                return $ x+1
  ret <- run $ myforM lis op
  out <- run $ readIORef r
  stop $ printTestCase ("forM "++show lis++" op") $
    conjoin [printTestCase "return value" $ ret === map (+1) lis,
             printTestCase "side effects" $ out === reverse lis]

prop_t13_tuplaKutsu = monadicIO $ do
  i <- pick $ (choose (0,20) :: Gen Int)
  let op = return (return i)
  out <- run $ tuplaKutsu $ op
  stop $ printTestCase ("doubleCall (return (return "++show i++"))") $
    out === i

prop_t14_compose = monadicIO $ do
  i <- pick $ (choose (0,20) :: Gen Int)
  let op1 = return . (*2)
      op2 = return . (+1)
  out <- run $ compose op1 op2 i
  stop $ printTestCase "compose (return . (*2)) (return . (+1))" $
    out === (i+1)*2

prop_t15_mkCounter = monadicIO $ do
  n <- pick $ choose (0,20)
  m <- run $ do (i,g) <- mkCounter
                replicateM_ n i
                g
  stop $ m === n

prop_t16_hFetchLines = monadicIO $ do
  lines <- pick $ listOf1 word
  inds <- fmap (nub.sort) . pick . listOf1 $ choose (1,length lines)

  dir <- run $ getTemporaryDirectory
  (path,h) <- run $ openTempFile dir "hFetchLines.in"
  run $ hPutStr h $ unlines lines
  run $ hSeek h AbsoluteSeek 0

  outs <- run $ hFetchLines h inds

  stop $ printTestCase ("hFetchLines h "++show inds++"\nContents:\n"++unlines lines) $
    conjoin [outs !! j === lines !! (i-1) | (j,i) <- zip [0..] inds]

toCSV = unlines . map (intercalate ",")

tmpSpit pattern conts = do
  dir <- getTemporaryDirectory
  (path,h) <- openTempFile dir pattern
  hPutStr h conts
  hClose h
  return path

prop_t17_readCSV = monadicIO $ do
  dat <- pick $ listOf1 (listOf1 word)
  let dat' = toCSV dat
  path <- run $ tmpSpit "readCSV.in" dat'
  ret <- run $ readCSV path
  stop $ printTestCase ("File contents: "++show dat') $ ret === dat

prop_t18_compareFiles = monadicIO $ do
  alines <- pick $ listOf1 word
  lines2 <- pick $ vectorOf (length alines) word
  diffs <- pick $ fmap (nub.sort) $ listOf1 (choose (0,length alines-1))
  let blines = [ if elem i diffs then s1++s2 else s1 | (i,s1,s2) <- zip3 [0..] alines lines2]
      ac = unlines alines
      bc = unlines blines
      should = concatMap (\i -> ["< "++alines!!i,"> "++alines!!i++lines2!!i]) diffs
  path1 <- run $ tmpSpit "compareFilesA.in" ac
  path2 <- run $ tmpSpit "compareFilesB.in" bc
  (outp,()) <- runc' $ compareFiles path1 path2
  let ls = lines outp
  stop $ printTestCase ("compareFiles\nFile A:\n"++ac++"File B:\n"++bc) $
    conjoin [printTestCase "number of lines printed" $ length ls === 2*length diffs,
             printTestCase "lines printed" $ ls === should]

prop_t19_interact_terminates = monadicIO $ do
  let f :: (String,String) -> (Bool,String,String)
      f (s,_) = (False,s,s)
  w <- pick $ word
  (text,ret) <- runc w $ interact' f ""
  stop $ conjoin [printTestCase "tulostus" $ text === w,
                  printTestCase "palautus" $ ret === w]

prop_t19_interact_loop = monadicIO $ do
  is <- pick $ listOf1 (arbitrary :: Gen Int)
  let f :: (String,[Int]) -> (Bool,String,[Int])
      f ("END",lis) = (False,"END\n", lis)
      f (x,lis)     = (True, "PICK\n", read x : lis)
      eret = reverse $ 0:is
      etext = unlines $ replicate (length is) "PICK" ++ ["END"]
  (text,ret) <- runc (unlines $ map show is ++ ["END"]) $ interact' f [0]
  stop $ conjoin [printTestCase "printing" $ text === etext,
                  printTestCase "return value" $ ret === eret]
