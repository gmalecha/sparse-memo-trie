{-# LANGUAGE TypeOperators, TypeFamilies, ScopedTypeVariables, RecordWildCards, CPP #-}
module SparseInt
     ( SparseWord32(..)
     , unsafeAccessed
     )
where

import Data.Word
import Data.Bits
import Data.IORef
import Data.Atomics
import Numeric

import qualified System.IO.Unsafe as UNSAFE

import Lib

#include "MachDeps.h"

newtype SparseWord32 = SW32 { unSparseWord32 :: Word32 }

data SW32Tree a = SIBranch !Word32 -- ^ Mask
                           !Word32 -- ^ Path
                           !Word32 -- ^ choice bit
                           (IORef (SW32Tree a)) -- ^ next bit is 0
                           (IORef (SW32Tree a)) -- ^ next bit is 1
                | SIValue !Word32 a -- ^ Exactly the value a
                | SIEmpty

instance Show a => Show (SW32Tree a) where
  show SIEmpty = "<empty>"
  show (SIValue w a) = "<Value:" ++ showHex w "" ++ "@" ++ show a ++ ">"
  show (SIBranch m p c l r) =
    "<Branch:" ++ showHex p "" ++ "?" ++ showHex c "" ++ ":" ++ show (UNSAFE.unsafePerformIO $ readIORef l) ++ "|" ++ show (UNSAFE.unsafePerformIO $ readIORef r) ++ ">"

showTIO :: Show a => SW32Tree a -> IO String
showTIO SIEmpty = return "<empty>"
showTIO (SIValue w a) = return $ "<Value:" ++ showHex w "" ++ "@" ++ show a ++ ">"
showTIO (SIBranch m p c l r) = do
  ls <- readIORef l >>= showTIO
  rs <- readIORef r >>= showTIO
  return $ "<Branch:" ++ showHex p "" ++ "?" ++ showHex c "" ++ ":" ++ ls ++ "|" ++ rs ++ ">"

showIO :: Show a => SparseWord32 :~>: a -> IO String
showIO SparseW32ST{..} = readIORef unSparseW32ST >>= showTIO

-- | Get the keys for the memoized entries.
-- NOTE: This operation is unsafe because it violates the contextual equivalence
-- that the @:~>:@ ensures.
unsafeAccessed :: SparseWord32 :~>: a -> [Word32]
unsafeAccessed (SparseW32ST _ tr) =
  doIt 0 $ UNSAFE.unsafePerformIO $ readIORef tr
  where
    doIt _ SIEmpty = []
    doIt z (SIValue k _) = [k .|. z]
    doIt z (SIBranch _ p b l r) =
      doIt (z .|. p) (UNSAFE.unsafePerformIO $ readIORef l) ++
      doIt (z .|. p .|. b) (UNSAFE.unsafePerformIO $ readIORef r)

instance HasSTrie SparseWord32 where
  data (:~>:) SparseWord32 a = SparseW32ST { fn :: Word32 -> a
                                           , unSparseW32ST :: IORef (SW32Tree a) }
  trie f = SparseW32ST (f . SW32) (UNSAFE.unsafePerformIO $ newIORef $ SIEmpty)
  untrie SparseW32ST{..} x = sw32lookup (unSparseWord32 x) fn unSparseW32ST


-- | Looks at the first two numbers builds a mask of the (first) bits that they have in common
-- and a mask for the first bit they have different
firstBitSet :: Word32 -> Word32 -> (Word32, Word32) -- ^ (same, diff)
firstBitSet a b =
  (bit - 1, bit)
  where
    diff = a `xor` b

    bit = get 1

    -- TODO: There needs to be a faster implementation of this
    get msk =
      if msk .&. diff == 0
      then get (msk `shiftL` 1)
      else msk

-- | Lookup a value in an @SW32Tree@.
sw32lookup :: forall a. Word32 -> (Word32 -> a) -> IORef (SW32Tree a) -> a
sw32lookup val f tr = UNSAFE.unsafePerformIO $ searchFor val tr
  where
    -- NOTE: bits in `x` that have already been checked are set to 0
    searchFor :: Word32 -> IORef (SW32Tree a) -> IO a
    searchFor x ref = do
      tkt <- readForCAS ref
      case peekTicket tkt of
        SIEmpty -> do
          let z = f val
          let br = SIValue x z
          (suc, ntkt) <- casIORef ref tkt br
          if suc
            then return z
            else do putStrLn "looping...(empty)"
                    searchFor x ref

        SIValue mask v ->
          if mask == x then return v
          else do
            let z = f val
            let (msk, chk) = firstBitSet mask x
            v' <- newIORef $ SIValue (mask .&. complement (msk .|. chk)) v
            z' <- newIORef $ SIValue (x .&. complement (msk .|. chk)) z
            let br = if mask .&. chk /= 0 then
                       SIBranch msk (msk .&. mask) chk z' v'
                     else
                       SIBranch msk (msk .&. mask) chk v' z'
            (suc, ntkt) <- casIORef ref tkt br
            if suc
              then return z
              else do putStrLn "looping...(value)"
                      searchFor x ref

        SIBranch mask path bit l r ->
          if x .&. mask == path then
            if x .&. bit == 0
            then searchFor (x .&. complement (mask .|. bit)) l
            else searchFor (x .&. complement (mask .|. bit)) r
          else do
            let z = f val
            let (msk, chk) = firstBitSet mask x
            v' <- newIORef $ SIBranch msk (path .&. complement (msk .|. chk)) bit l r
            z' <- newIORef $ SIValue (x .&. complement (msk .|. chk)) z
            let br = if mask .&. chk /= 0 then
                       SIBranch msk (msk .&. mask) chk z' v'
                     else
                       SIBranch msk (msk .&. mask) chk v' z'
            (suc, ntkt) <- casIORef ref tkt br
            if suc
              then return z
              else do putStrLn "looping...(branch)"
                      searchFor x ref

newtype SparseWord64 = SW64 { unSparseWord64 :: Word64 }

instance HasSTrie SparseWord64 where
  data (:~>:) SparseWord64 a = SparseW64ST { fn64 :: Word64 -> a
                                           , unSparseW64ST :: IORef (SW32Tree (Word32 :~>: a)) }
  trie f = SparseW64ST (f . SW64) (UNSAFE.unsafePerformIO $ newIORef $ SIEmpty)
  untrie SparseW64ST{..} (SW64 x) =
    untrie (sw32lookup h (\ h -> trie $ \ l -> fn64 (h `glue323264` l)) unSparseW64ST) l
    where
      l = lowByte32 x
      h = highByte32 x

#if (WORD_SIZE_IN_BITS == 32)

#elif (WORD_SIZE_IN_BITS == 64)

#endif /* WORD_SIZE_IN_BITS */
