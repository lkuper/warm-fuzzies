-- file: BloomFilter/Internal.hs
module BloomFilter.Internal
    (
      Bloom(..)
    , MutBloom(..)
    ) where

import Data.Array.ST (STUArray)
import Data.Array.Unboxed (UArray)
import Data.Word (Word32)

-- For this data type definition, we're using _record syntax_.  When
-- we write it this way, we automatically get blmHash and blmArray
-- accessor functions.  B is the name of the _value constructor_,
-- which is a function that creates and returns values of the Bloom
-- type.

-- It would also be fine (and normal) for the value constructor (B) to
-- have the same name as its type constructor (Bloom).  There's no
-- ambiguity, because type constructors can only appear in types, and
-- value constructors can only appear in expressions.
data Bloom a = B {
      blmHash  :: (a -> [Word32])
    , blmArray :: UArray Word32 Bool
    }

-- Record syntax again for this one.
data MutBloom s a = MB {
      mutHash :: (a -> [Word32])
    , mutArray :: STUArray s Word32 Bool
    }