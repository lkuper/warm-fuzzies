import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies
import Control.DeepSeq

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f

    -- Divide the list of sudoku problems to solve into two equal (or
    -- almost-equal) parts.
    let (as,bs) = splitAt (length grids `div` 2) grids

    -- 'evaluate' will evaluate its argument to WHNF.  (I think we
    -- need to force this evaluation because, since we're not doing
    -- anything with the result, GHC would just refrain from doing it
    -- otherwise?)
        
    -- Eval is the monad within which rpar and rseq return their
    -- results.  runEval runs the Eval computation.
    evaluate $ runEval $ do
      -- This says that (deep (map solve as)) can run at the same time
      -- as something else (i.e., can be parallelized).
      a <- rpar (deep (map solve as))
      
      -- (deep (map solve bs)) can also be run at the same time as
      -- something else.
      b <- rpar (deep (map solve bs))
      
      -- Now fire off the two computations.
      rseq a
      rseq b
      
      return ()

-- deep evaluates its argument to normal form and returns it.  We need
-- to do this to ensure that our two subtasks are each accomplishing
-- something.  In fact, the Par monad has this behavior by default.
deep :: NFData a => a -> a
deep a = deepseq a a
