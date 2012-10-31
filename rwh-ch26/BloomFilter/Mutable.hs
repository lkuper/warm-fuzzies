-- file: BloomFilter/Mutable.hs
module BloomFilter.Mutable
    (
      MutBloom
    -- , elem
    -- , notElem
    -- , insert
    -- , length
    , new
    ) where

import Control.Monad (liftM)
import Control.Monad.ST (ST)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.Word (Word32)
import Prelude hiding (elem, length, notElem)

import BloomFilter.Internal (MutBloom(..))

new :: (a -> [Word32]) -> Word32 -> ST s (MutBloom s a)

-- The MB value constructor is defined in Internal.hs.  It needs a
-- hash function and an array.  For the hash function, we just pass
-- along the one we were given.

-- newArray is defined in Data.Array.MArray; it's a method provided by
-- the MArray interface.  But it's a monadic value because newArray
-- returns a monad-wrapped thing.  We want to apply 'MB hash' (which
-- is the function that we get from applying 'MB' to 'hash', and which
-- is still waiting to get an array) to the thing inside the monad.
-- That's what 'liftM' does here: it "lifts" 'MB hash' to the level of
-- the monad, so that it can be applied to the thing inside the monad.
-- The result is the value of type MB, which is what we wanted -- but
-- it's still inside the monad, which is good because the type of
-- 'new' calls for something that is wrapped in ST.
new hash numBits = MB hash `liftM` newArray (0,numBits-1) False