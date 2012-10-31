-- | Stuff from chapter 24 of "Real World Haskell". |

module Main where

import Control.Parallel (par, pseq)
import Control.Exception
import Control.DeepSeq hiding (force) -- don't import force from here,
                                      -- because we define our own
import System.Random
import System.Environment
import Text.Printf
import Data.Time.Clock

-- A parallel sort function.
parSort :: (Ord a) => [a] -> [a]

-- | What 'force', 'par', and 'pseq' do here:
--
--   * the 'par' makes 'force greater' get eval'd in parallel with
--   (force lesser ...).
--
--   * the 'pseq' makes 'force lesser' get eval'd before returning
--   (lesser ++ x:greater).
--
--   * the 'force' makes the evaluations cover the whole spine of the
--   lists 'greater' and 'lesser'. | 
parSort (x:xs)    = force greater `par` (force lesser `pseq`
                                         (lesser ++ x:greater))
                    
-- | This part's identical to sequential sort. | 
    where lesser  = parSort [y | y <- xs, y <  x]
          greater = parSort [y | y <- xs, y >= x]
parSort _         = []

-- | A sequential sort, for comparison's sake. |
sort :: (Ord a) => [a] -> [a]
sort (x:xs) = lesser ++ x:greater
    where lesser  = sort [y | y <- xs, y <  x]
          greater = sort [y | y <- xs, y >= x]
sort _ = []

-- | The force function. |
force :: [a] -> ()
force xs = go xs `pseq` ()
    where go (_:xs) = go xs
          go [] = 1
          
-- | Examples:
-- sort [2,4,4,2,4,3,1,5]
-- parSort [2,4,4,2,4,3,1,5]
          
-- | For testing purposes, we want a function that produces a list of
-- n random integers to be sorted.  The 'mkStdGen' function is
-- provided by System.Random and returns a RandomGen.  The 'randoms'
-- function takes a RandomGen and returns an infinite list of random
-- values, of which we take an n-element prefix.
genRandoms :: Int -> [Int]
genRandoms n = take n $ randoms (mkStdGen 120) :: [Int]          
          
-- | Adding a 'main' function makes this usable and testable as a
-- standalone executable.  
-- 
-- getArgs returns a list of the program's command-line arguments,
-- wrapped in the IO monad.
main = do args <- getArgs
          -- It's OK to mix in non-monadic code, like this 'let'
          -- expression.
          let size =
                case args of
                  -- If no argument is specified, default to an array
                  -- size of 2^8 = 256.
                  []  -> 2^8
                  [n] -> 2^(read n)
          
          let rands = genRandoms size
	  evaluate (deepseq rands ())
          putStrLn "Pseq based version:"

          start <- getCurrentTime
          let sorted = parSort rands
          putStr "Prefix of sorted list:\n  "
          print$ take 8 $ sorted
          end   <- getCurrentTime

          let runningTime = ((fromRational $ toRational $ diffUTCTime end start) :: Double)
          printf "Sorting AList took %0.3f sec.\n" runningTime
          putStrLn $ "SELFTIMED " ++ show runningTime
