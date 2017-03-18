module Main where

import Control.Monad
import Data.Word
import Foreign.Storable
import Criterion.Main
import qualified Data.MemoTrie as M
import qualified Lib as S
import SparseInt

type Trial = Word32

foo :: Trial -> Int
foo x = 2 * fromIntegral x

val :: [Trial]
val = [0..10] ++ [0..20] ++ [0..30] ++ [0..50] ++ [0..60] ++ [0..70] ++ [0..70]
val2 = val ++ val


eval :: (Trial -> Int) -> [Trial] -> Int
eval f val = doIt 0 val
  where
    doIt acc [] = acc
    doIt acc (x:xs) = doIt (acc + f x) xs

-- | Construct a random list of length @n@
randomList :: Int -> IO [Int]
randomList n = mapM gen [1..n]
  where
    gen :: Int -> IO Int
    gen = const $ return 0

-- | Benchmark a bunch of results.
main :: IO ()
main = do
  l1000 <- fmap fromIntegral <$> randomList 1000
  l5000 <- fmap fromIntegral <$> randomList 5000
  l10000 <- fmap fromIntegral <$> randomList 10000
  defaultMain
    [ bench "flush" $ whnf sum val
    , bgroup "val" $ compareList val
    , bgroup "random 1000" $ compareList l1000
    , bgroup "random 5000" $ compareList l5000
    , bgroup "random 10000" $ compareList l10000
    ]
  where
    compareList val =
      [ bench "HasTrie"
        $ whnf (eval $ M.untrie $ M.trie foo) val
      , bench "HasSTrie (dense)"
        $ whnf (eval $ S.untrie $ S.trie foo) val
      , bench "HasSTrie (sparse)"
        $ whnf (eval $ (S.untrie $ S.trie (foo . unSparseWord32)) . SW32) val
      ]
