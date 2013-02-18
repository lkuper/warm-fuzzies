import Sudoku
import Control.Exception
import System.Environment
import Control.Monad.Par hiding (parMap)

main :: IO ()
main = do
    [f] <- getArgs
    grids <- fmap lines $ readFile f
    evaluate $ runPar $ do
      ivs <- parMap solve grids
      mapM get ivs 
    return ()

-- parMap will create one IVar for each item in the list it's mapping
-- over and fork off a computation that will fill in that IVar.
parMap :: NFData b => (a -> b) -> [a] -> Par [IVar b]
parMap f [] = return []
parMap f (a:as) = do
  -- Create a new IVar to put the result in.
  v <- new
  -- Fork a computation that computes (f a) (in this case, calling
  -- "solve" on a puzzle) and writes the result into v.
  fork $ put v (f a)
  -- Recursive call.
  bs <- parMap f as
  
  return (v:bs)
