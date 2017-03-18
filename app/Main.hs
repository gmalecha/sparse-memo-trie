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


eval :: (Trial -> Int) -> Int
eval f = doIt 0 val
  where
    doIt acc [] = acc
    doIt acc (x:xs) = doIt (acc + f x) xs

benchTrie :: IO ()
benchTrie =
  defaultMain
    [ bench "flush" $ whnf sum val
    , bgroup "HasTrie" [ bench "val" $ whnf eval (M.untrie $ M.trie foo) ]
    , bgroup "HasSTrie" [ bench "val" $ whnf eval (S.untrie $ S.trie foo) ]
    , bgroup "HasSTrie(sparse)"
      [ bench "val" $ whnf eval ((S.untrie $ S.trie (foo . unSparseWord32)) . SW32) ]
    ]


main :: IO ()
main = do
  benchTrie
  -- -- benchTrie
  -- -- putStrLn "starting..."
  -- let tr = S.trie (foo . unSparseWord32)
  -- -- showIO tr >>= putStrLn
  -- -- _ <- return $ (S.untrie tr $ SW32 0 == 0)
  -- -- showIO tr >>= putStrLn
  -- -- _ <- return $ (S.untrie tr $ SW32 1 == 2)
  -- -- showIO tr >>= putStrLn
  -- -- _ <- return $ (S.untrie tr $ SW32 2 == 4)
  -- -- showIO tr >>= putStrLn
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- checkL tr val
  -- where
  --   checkL tr [] = return () -- showIO tr >>= putStrLn
  --   checkL tr (x : xs) = do
  --     -- showIO tr >>= putStrLn
  --     -- putStrLn $ "adding " ++ show x
  --     guard $ (S.untrie tr $ SW32 x) == foo x
  --     checkL tr xs
