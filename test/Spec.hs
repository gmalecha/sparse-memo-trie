{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
import Control.Parallel.Strategies
import Data.Typeable
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import qualified Lib as S


values :: forall n. (Enum n, Num n) => [n]
values =
  [0..10] ++ [0..20] ++ [0..30] ++ [0..40] ++
  [-10..0] ++ [-20..0] ++ [-30..0] ++ [-40..0]

smemo :: S.HasSTrie a => (a -> b) -> (a -> b)
smemo = S.untrie . S.trie

sequentialTests =
  testGroup "Sequential"
  [ testSeqNum @Word8
  , testSeqNum @Word16
  , testSeqNum @Word32
  , testSeqNum @Word64
  , testSeqNum @Int
  ]
  where
    testSeqNum :: forall n. (S.HasSTrie n, NFData n, Eq n, Show n, Typeable n, Enum n, Num n) => TestTree
    testSeqNum =
      testGroup (show $ typeRep (Proxy :: Proxy n))
      [ testCase "id"
        $ assertEqual "id" vs (smemo id <$> vs)
      , testCase "+1"
        $ assertEqual "(+1)" (((+) 1) <$> vs) (smemo ((+) 1) <$> vs)
      ]
      where
        vs = values @n

parallelTests =
  testGroup "Parallel"
  [ testParNum @Word8
  , testParNum @Word16
  , testParNum @Word32
  , testParNum @Word64
  , testParNum @Int
  ]
  where
    testParNum :: forall n. (S.HasSTrie n, NFData n, Eq n, Show n, Typeable n, Enum n, Num n) => TestTree
    testParNum =
      testGroup (show $ typeRep (Proxy :: Proxy n))
      [ testCase "id"
        $ assertEqual "id" vs
          $ withStrategy (parList rdeepseq) (smemo id <$> vs)
      , testCase "+1"
        $ assertEqual "(+1)" (((+) 1) <$> vs)
          $ withStrategy (parList rdeepseq) (smemo ((+) 1) <$> vs)
      ]
      where
        vs = values @n

main :: IO ()
main = defaultMain
  $ testGroup "all" [sequentialTests, parallelTests]
