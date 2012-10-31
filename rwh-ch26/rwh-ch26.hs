-- | Stuff from chapter 26 of "Real World Haskell". |


-- | My goal here is to understand the ST monad, and ST-like things,
-- better, so that I'll be able to understand how to glom the Par
-- monad together with ST to accomplish disjoint, in-place, parallel
-- array update.  If I learn about Bloom filters along the way, so
-- much the better.

import BloomFilter.Mutable