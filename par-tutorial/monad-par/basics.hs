-- | First steps with the Par monad.  This is using the from-git
-- version I have on my machine, so it might not have the same
-- behavior as the released version.
-- 
-- The following information all comes from the Simon Marlow
-- "Parallelism and Concurrency in Haskell" tutorial.
-- 
-- The Par interface looks like this:
-- 
--   newtype Par a
--   instance Functor Par
--   instance Applicative Par
--   instance Monad Par
-- 
-- runPar fires up a new scheduler instance consisting of one worker
-- thread per processor.  It returns a pure result.
-- 
--   runPar :: Par a -> a
-- 
-- fork creates parallel tasks.  The computation passed as the
-- argument to fork (the "child") is executed concurrently with the
-- current computation (the "parent").
-- 
--   fork :: Par () -> Par ()
-- 
-- We can communicate between parallel computations using the IVar
-- type and its operations:
-- 
--   data IVar a
--   -- instance Eq
-- 
-- new creates a new IVar, which is initially empty.
-- 
--   new :: Par ( IVar a )
-- 
-- put fills an IVar with a value.  Multiple puts are an error.
-- 
--   put :: NFData a = > IVar a -> a -> Par ()
-- 
-- get retrieves the value of an IVar, waiting until one has been put
-- if necessary.
-- 
--   get :: IVar a -> Par a
-- 
-- Together, fork and IVars allow the construction of dataflow
-- networks.  For each edge in the graph, we make an IVar.  For each
-- node in the graph, we call fork.  The code for each node calls get
-- on each input and put on each output of the node.  The order of the
-- fork calls is irrelevant.
-- 
-- We can also express patterns other than dataflow networks.

import Control.Monad -- get liftM from here
-- Hide spawn and parMapM because we're going to define our own.
import Control.Monad.Par hiding (spawn, parMapM)

-- Here's a simple abstraction for a parallel computation that returns
-- a result.  This takes care of the creation of IVars and "put" for
-- us.  It leaves "get" for another computation to do.
spawn :: NFData a => Par a -> Par ( IVar a )
spawn p = do
  i <- new -- create a new, empty IVar
  fork $ do -- spawn a computation that evaluates the argument, p,
            -- binds it to x, and writes the result into the IVar i.
    x <- p  -- (I think that we need this "x <- p" step in here,
            -- rather than just "put i p", because then p would never
            -- be evaluated.  But we could have avoided that by
            -- writing "p >>= put r", I think.)
    put i x
  return i -- return the result in the Par monad


-- parMapM takes a function and a list and spawns one task that runs
-- that function for each member of the list.  The result comes back
-- in the Par monad; here "ibs" is short for "list of IVars containing
-- things of type Par b".  So, to get it out of the Par monad, we'd
-- need to use runPar.
parMapM :: NFData b => ( a -> Par b ) -> [ a ] -> Par [ b ]
parMapM f as = do
  ibs <- mapM ( spawn . f ) as
  mapM get ibs

-- | Let's say that we want to compute the factorials of a list of
-- numbers in parallel.  You know, the usual stuff.  But since fact is
-- a pure function, we need a version of parMapM that takes a pure
-- function.  (Luckily, there's one built in; it's just called
-- parMap.)

fact :: Int -> Int
fact 0 = 1
fact n = n * (fact (n - 1))

main :: IO ()
main = do
  print $ runPar $ do
    factorials <- parMap fact [12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12]
    return factorials
  return ()

