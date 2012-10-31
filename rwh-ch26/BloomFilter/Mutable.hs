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

-- I'm not exactly sure what's going on here.  newArray is defined in
-- Data.Array.MArray; it's a method provided by the MArray interface.
-- I'm not sure what is being "lift"ed here.  Do we have to lift the
-- result of newArray into the ST monad?  Or are we lifting "hash"?
new hash numBits = MB hash `liftM` newArray (0,numBits-1) False