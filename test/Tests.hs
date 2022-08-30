{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Sequence as S
import Torsor
import Torsor.Collections
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [differenceableTests]

differenceableTests :: TestTree
-- differenceableTests = testGroup "Diffable" [intTests, floatTests, doubleTests, sumTests, mapTests, intMapTests, sequenceTests, maybeTests]
differenceableTests = testGroup "Torsor" [intTests, mapTests, intMapTests, sequenceTests]

intTests :: TestTree
intTests =
  testGroup
    "Ints"
    [ QC.testProperty "Difference law" $ \path (old :: Int) -> difference (add old path) old == path,
      QC.testProperty "Add law" $ \new old -> add old (difference new old) == (new :: Int),
      QC.testProperty "Path Law" $ \old middle new -> add old (difference middle old + difference new middle) == (new :: Int)
    ]

mapTests :: TestTree
mapTests =
  testGroup
    "Maps"
    [ QC.testProperty "Add law" $ \new old -> add (difference new old) old == (new :: M.Map Int Int),
      QC.testProperty "Path Law" $ \old middle new -> add (difference middle old <> difference new middle) old == (new :: M.Map Int Int)
    ]

intMapTests :: TestTree
intMapTests =
  testGroup
    "IntMaps"
    [ QC.testProperty "Add law" $ \new old -> add (difference new old) old == (new :: IM.IntMap Int),
      QC.testProperty "Path Law" $ \old middle new -> add (difference middle old <> difference new middle) old == (new :: IM.IntMap Int)
    ]

sequenceTests :: TestTree
sequenceTests =
  testGroup
    "Sequences"
    [ QC.testProperty "Add law" $ \new old -> add (difference new old) old == (new :: S.Seq Int),
      QC.testProperty "Path Law" $ \old middle new -> add (difference middle old <> difference new middle) old == (new :: S.Seq Int)
    ]

-- maybeTests :: TestTree
-- maybeTests =
--   testGroup
--     "Maybe"
--     [ QC.testProperty "Add law" $ \new old -> add old (difference new old) == (new :: Maybe (Sum Int)),
--       QC.testProperty "Path Law" $ \old middle new -> add old (difference middle old <> difference new middle) == (new :: Maybe (Sum Int))
--     ]

eqDouble :: (Ord a, Fractional a) => a -> a -> Bool
eqDouble 0 b = b == 0
eqDouble a b = (a - b) / a - 1 < 1e-7
