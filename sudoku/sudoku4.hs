import Sudoku
import Control.Exception
import System.Environment
import Control.Parallel.Strategies hiding (parMap)
import Control.Seq as Seq
import Control.DeepSeq

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    
    -- We don't actually need the length of grids; this is just a
    -- trick to force grids to be evaluated entirely before we start
    -- the parallel part of our program.  It's not necessary, but it
    -- lets us divide up the sequential and parallel parts.
    evaluate (length grids)
    
    evaluate $ deep $ runEval $ parMap solve grids
    --   better: evaluate $ Seq.seqList Seq.rseq $ runEval $ parMap solve grids
    --   better: evaluate $ deep $ (map solve grids `using` parList rwhnf)
    --   better: mapM_ evaluate (map solve grids `using` parList rwhnf)
    return ()

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)

deep :: NFData a => a -> a
deep a = deepseq a a
