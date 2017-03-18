{-# LANGUAGE TypeOperators, TypeFamilies, CPP #-}
module Lib
where

import Data.Word
import Data.Bits

import Debug.Trace

#include "MachDeps.h"

class HasSTrie t where
  data (:~>:) t a
  trie :: (t -> a) -> t :~>: a
  untrie ::  t :~>: a -> t -> a


data Quad a = Q a a a a

qextr :: Word8 -> Quad a -> a
qextr w (Q a b c d)
  | m == 0 = a
  | m == 1 = b
  | m == 2 = c
  | m == 3 = d
  | trace ("m = " ++ show m) True = undefined
  where
    m = w .&. 0x3


instance HasSTrie Word8 where
  data (:~>:) Word8 a = W8ST { unW8ST :: Quad (Quad (Quad (Quad a))) }

  trie f = W8ST $ go (go (go (go (f . fromIntegral)))) 0
    where
      go :: (Int -> a) -> Int -> Quad a
      go k i = Q (k (i * 4)) (k (i * 4 + 1)) (k (i * 4 + 2)) (k (i * 4 + 3))

  untrie (W8ST x) v =
    qextr v. qextr (v `shiftR` 2) . qextr (v `shiftR` 4) . qextr (v `shiftR` 6) $ x

highByte , lowByte :: Word16 -> Word8
highByte = lowByte . byteSwap16
lowByte = fromIntegral . ( .&. 0xff)

glue8816 :: Word8 -> Word8 -> Word16
glue8816 h l = ((fromIntegral h) `shiftL` 8) .|. fromIntegral l

instance HasSTrie Word16 where
  data (:~>:) Word16 a = W16ST { unW16ST :: Word8 :~>: (Word8 :~>: a) }
  trie f = W16ST $ trie $ \ l -> trie $ \ h -> f (h `glue8816` l)
  untrie (W16ST m) x = untrie (untrie m (lowByte x)) (highByte x)

highByte16 , lowByte16 :: Word32 -> Word16
highByte16 = lowByte16 . byteSwap32
lowByte16 = fromIntegral . ( .&. mask)
  where
    mask = 0xffff

glue161632 :: Word16 -> Word16 -> Word32
glue161632 h l = ((fromIntegral h) `shiftL` 16) .|. fromIntegral l

instance HasSTrie Word32 where
  data (:~>:) Word32 a = W32ST { unW32ST :: Word16 :~>: (Word16 :~>: a) }
  trie f = W32ST $ trie $ \ l -> trie $ \ h -> f (h `glue161632` l)
  untrie (W32ST m) x = untrie (untrie m (lowByte16 x)) (highByte16 x)

highByte32 , lowByte32 :: Word64 -> Word32
highByte32 = lowByte32 . byteSwap64
lowByte32 = fromIntegral . ( .&. mask)
  where
    mask = 0xffffffff

glue323264 :: Word32 -> Word32 -> Word64
glue323264 h l = ((fromIntegral h) `shiftL` 32) .|. fromIntegral l

instance HasSTrie Word64 where
  data (:~>:) Word64 a = W64ST { unW64ST :: Word32 :~>: (Word32 :~>: a) }
  trie f = W64ST $ trie $ \ l -> trie $ \ h -> f (h `glue323264` l)
  untrie (W64ST m) x = untrie (untrie m (lowByte32 x)) (highByte32 x)


#if (WORD_SIZE_IN_BITS == 32)

instance HasSTrie Int where
  newtype (:~>:) Int a = IntST { unIntST :: Word32 :~>: a }
  trie f = IntST $ trie $ f . fromIntegral
  untrie (IntST t) = untrie t . fromIntegral

#elif (WORD_SIZE_IN_BITS == 64)

instance HasSTrie Int where
  newtype (:~>:) Int a = IntST { unIntST :: Word64 :~>: a }
  trie f = IntST $ trie $ f . fromIntegral
  untrie (IntST t) = untrie t . fromIntegral

#endif /* WORD_SIZE_IN_BITS */
