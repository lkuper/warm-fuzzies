-- | Stuff from chapter 24 of "Real World Haskell". |

module Sorting where

import Control.Parallel (par, pseq)

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
          


