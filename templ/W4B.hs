module W4 where

import Control.Monad
import Data.List
import Data.IORef
import System.IO

-- Week 4:
--   * The IO type
--   * do-notation
--
-- Useful functions / operations:
--   * putStrLn
--   * getLine
--   * readLn
--   * replicateM
--   * readFile
--   * lines
--
-- If these exercises feel weird or hard, feel free to skip this week for now

-- Ex 1: define an IO operation hello that prints two lines. The
-- first line should be HELLO and the second one WORLD

hello :: IO ()
#ifdef sol
hello = do putStrLn "HELLO"
           putStrLn "WORLD"
#else
hello = undefined
#endif

-- Ex 2: define the IO operation greet that takes a name as an
-- argument and prints a line "HELLO name".

greet :: String -> IO ()
#ifdef sol
greet name = putStrLn $ "HELLO " ++ name
#else
greet name = undefined
#endif


-- Ex 3: define the IO operation greet2 that reads a name from the
-- keyboard and then greets that name like the in the previous
-- exercise.
--
-- Try to use the greet operation in your solution.

greet2 :: IO ()
#ifdef sol
greet2 = do name <- getLine
            greet name
#else
greet2 = undefined
#endif

-- Ex 4: define the IO operation readWords n which reads n lines from
-- the user and returns them in alphabetical order.

readWords :: Int -> IO [String]
#ifdef sol
readWords n = do words <- replicateM n getLine
                 return $ sort words
#else
readWords n = undefined
#endif

-- Ex 5: define the IO operation readUntil f, which reads lines from
-- the user and returns them as a list. Reading is stopped when f
-- returns True for a line. (The value for which f returns True is not
-- returned.)

readUntil :: (String -> Bool) -> IO [String]
#ifdef sol
readUntil f = do word <- getLine
                 if f word
                   then return []
                   else do words <- readUntil f
                           return $ word:words
#else
readUntil f = undefined
#endif

-- Ex 6: given n, print the n first fibonacci numbers, one per line

printFibs :: Int -> IO ()
#ifdef sol
printFibs n = mapM_ print $ fibs 0 1 n
  where fibs a b 0 = []
        fibs a b n = b:fibs b (a+b) (n-1)
#else
printFibs n = undefined
#endif

-- Ex 7: isums n should read n numbers from the user and return their
-- sum. Additionally, after each read number, the sum up to that
-- number should be printed.

isums :: Int -> IO Int
#ifdef sol
isums n = go 0 n
  where go sum 0 = return sum
        go sum n = do i <- readLn
                      let sum' = sum+i
                      print sum'
                      go sum' (n-1)
#else
isums n = undefined
#endif

-- Ex 8: when is a useful function, but its first argument has type
-- Bool. Write a function that behaves similarly but the first
-- argument has type IO Bool.

whenM :: IO Bool -> IO () -> IO ()
#ifdef sol
whenM cond op = do b <- cond
                   when b op
#else
whenM cond op = undefined
#endif

-- Ex 9: implement the while loop. while condition operation should
-- run operation as long as condition returns True.
--
-- Examples:
-- while (return False) (putStrLn "IMPOSSIBLE")  -- prints nothing
--
-- let ask :: IO Bool
--     ask = do putStrLn "Y/N?"
--              line <- getLine
--              return $ line == "Y"
-- in while ask (putStrLn "YAY!")
--
-- This prints YAY! as long as the user keeps answering Y

while :: IO Bool -> IO () -> IO ()
#ifdef sol
while cond op = whenM cond $ do op
                                while cond op
#else
while cond op = undefined
#endif

-- Ex 10: given a string and an IO operation, print the string, run
-- the IO operation, print the string again, and finally return what
-- the operation returned.
--
-- Note! the operation should be run only once
--
-- Examples:
--   debug "CIAO" (return 3)
--     - prints two lines that contain CIAO
--     - returns the value 3
--   debug "BOOM" getLine
--     1. prints "BOOM"
--     2. reads a line from the user
--     3. prints "BOOM"
--     4. returns the line read from the user

debug :: String -> IO a -> IO a
#ifdef sol
debug s op = do
  putStrLn s
  ret <- op
  putStrLn s
  return ret
#else
debug s op = undefined
#endif

-- Ex 11: Reimplement mapM_ (specialized to the IO type) using
-- recursion and pattern matching.
--
-- In case you don't know what mapM_ does, it takes a parameterized IO
-- operation and a list of parameters, and runs the operation for each
-- value in the list.

mymapM_ :: (a -> IO b) -> [a] -> IO ()
#ifdef sol
mymapM_ f [] = return ()
mymapM_ f (x:xs) = do f x
                      mymapM_ f xs
#else
mymapM_ = undefined
#endif

-- Ex 12: Reimplement the function forM using pattern matching and
-- recursion.

myforM :: [a] -> (a -> IO b) -> IO [b]
#ifdef sol
myforM []     f = return []
myforM (a:as) f = do b <- f a
                     bs <- myforM as f
                     return $ b : bs
#else
myforM as f = undefined
#endif

-- Ex 13: sometimes one bumps into IO operations that return IO
-- operations. For instance the type IO (IO Int) means an IO operation
-- that returns an IO operation that returns an Int.
--
-- Implement the function doubleCall which takes an operation op and
--   1. runs op
--   2. runs the operation returned by op
--   3. returns the value returned by this operation
--
-- Examples:
--   - doubleCall (return (return 3)) is the same as return 3
--
--   - let op :: IO (IO [String])
--         op = do l <- readLn
--                 return $ replicateM l getLine
--     in doubleCall op
--
--     works just like
--
--     do l <- readLn
--        replicateM l getLine

doubleCall :: IO (IO a) -> IO a
#ifdef sol
doubleCall op = do op2 <- op
                   op2
#else
doubleCall op = undefined
#endif

-- Ex 14: implement the analogue of function composition (the (.)
-- operator) for IO operations. That is, take an operation op1 of type
--     a -> IO b
-- an operation op2 of type
--     c -> IO a
-- and a value of type
--     c
-- and returns an operation op3 of type
--     IO b
--
-- op3 should of course
--   1. take the value of type c and pass it to op2
--   2. take the resulting value (of type a) and pass it to op1
--   3. return the result (of type b)

compose :: (a -> IO b) -> (c -> IO a) -> c -> IO b
#ifdef sol
compose op1 op2 c = do a <- op2 c
                       op1 a
#else
compose op1 op2 c = undefined
#endif

-- Ex 15: take a look at the documentaiton for Data.IORef
-- <http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-IORef.html>
--
-- Implement the function mkCounter that returns the io operations
-- inc :: IO () and get :: IO Int. These operations should work like this:
--
--   get returns the number of times inc has been called
--
-- In other words, a simple stateful counter.
--
-- An example of how mkCounter works in GHCi:
--
--  *W4> (inc,get) <- mkCounter
--  *W4> inc
--  *W4> inc
--  *W4> get
--  2
--  *W4> inc
--  *W4> inc
--  *W4> get
--  4

mkCounter :: IO (IO (), IO Int)
#ifdef sol
mkCounter = do
  ref <- newIORef 0
  let get = readIORef ref
      inc = modifyIORef ref (+1)
  return (inc,get)
#else
mkCounter = undefined
#endif

-- Ex 16: fetch from the given file (Handle) the lines with the given
-- indices. Line indexing starts from 1. You can assume that the
-- numbers are given in ascending order.
--
-- Have a look at the docs for the System.IO module for help.

hFetchLines :: Handle -> [Int] -> IO [String]
#ifdef sol
hFetchLines h nums = do cont <- hGetContents h
                        let split = lines cont
                        return $ pick 1 nums split
  where pick _ []       _         = []
        pick _ _        []        = []
        pick i (n:nums) (s:split)
          | i==n      = s:pick (i+1) nums split
          | otherwise = pick (i+1) (n:nums) split
#else
hFetchLines h nums = undefined
#endif

-- Ex 17: CSV is a file format that stores a two-dimensional array of
-- values in a file. Each row of the file is a row of the array. Each
-- row of the file consists of values on that row separated with the ,
-- character.
--
-- Implement the function readCSV that reads a CSV file and returns it
-- as a list of lists.
--
-- NB! You don't need to handle the intricacies of real CSV, e.g.
-- quoting. You can assume each , character starts a new field.
--
-- NB! The lines might have different numbers of elements.

readCSV :: FilePath -> IO [[String]]
#ifdef sol
readCSV path = do str <- readFile path
                  return $ map process $ lines str
  where process xs = case break (==',') xs of (a,[])    -> [a]
                                              (a,',':b) -> a:process b
#else
readCSV path = undefined
#endif

-- Ex 18: your task is to compare two files, a and b. The files should
-- have the same contents, but if lines at index i differ from each
-- other, you should print
--
-- < file a version of the line
-- > file b version of the line
--
-- Example:
--
-- File a contents:
-- a
-- aa
-- x
-- aa
-- bb
-- cc
--
-- File b contents:
-- a
-- aa
-- bb
-- aa
-- cc
-- dd
--
-- Output:
-- < x
-- > bb
-- < bb
-- > cc
-- < cc
-- > dd
--
-- NB! You can assume the files have the same number of rows.
--
-- Hint! It's probably wise to implement a pure function for finding
-- the differing lines. A suitable type could be
-- [String] -> [String] -> [String].

compareFiles :: FilePath -> FilePath -> IO ()
#ifdef sol
compareFiles a b = do ac <- readFile a
                      bc <- readFile b
                      mapM_ putStrLn $ compareHelper (lines ac) (lines bc)

compareHelper []     []     = []
compareHelper (a:as) (b:bs)
  | a /= b    = ("< "++a):("> "++b):compareHelper as bs
  | otherwise = compareHelper as bs
#else
compareFiles a b = undefined
#endif

-- Ex 19: In this exercise we see how a program can be split into a
-- pure part that does all of the work, and a simple IO wrapper that
-- drives the pure logic.
--
-- Implement the function interact' that takes a pure function f of
-- type
--   (String, st) -> (Bool, String, st)
-- and a starting state of type st and returns an IO operation of type
-- IO st
--
-- interact' should read a line from the user, feed the line and the
-- current state to f. f then returns a boolean, a string to print and
-- a new state. The string is printed, and if the boolean is True, we
-- continue running with the new state. If the boolean is False, the
-- execution has ended and the state should be returned.
--
-- Example:
--
-- let f :: (String,Integer) -> (Bool,String,Integer)
--     f ("inc",n)   = (True,"",n+1)
--     f ("print",n) = (True,show n,n)
--     f ("quit",n)  = (False,"bye bye",n)
-- in interact' f 0
--

interact' :: ((String,st) -> (Bool,String,st)) -> st -> IO st
#ifdef sol
interact' f state = do
  inp <- getLine
  case f (inp,state) of
    (True,  out, state') ->
      do putStr out
         interact' f state'
    (False, out, state') ->
      do putStr out
         return state'
#else
interact' f state = undefined
#endif
