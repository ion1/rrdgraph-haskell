{-
rrdgraph-haskell – Haskell DSL for rendering RRD graphs using RRDtool

Copyright © 2011 Johan Kiviniemi <devel@johan.kiviniemi.name>

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
-}

-- The only orphan instances are for the module being tested.
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Data.RRDGraph.Tests.CDef
where

import Prelude hiding (LT, EQ, GT)

import Data.RRDGraph.CDef

import Control.Applicative
import Data.Record.Label
import Data.Monoid

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.QuickCheck

instance Arbitrary CDef where
  arbitrary = arb 4
    where
      arb :: Integer -> Gen CDef
      arb depth | depth > 0 =
        let depth' = depth-1
        in  oneof [ arbConstant, arbOp depth' ]
      arb _ = arbConstant

      arbConstant = frequency [ (2, liftA Constant arbitrary), (1, arbData) ]

      arbOp depth = oneof
        [ COp0 <$> arbitrary
        , COp1 <$> arbitrary <*> arb depth
        , COp2 <$> arbitrary <*> arb depth <*> arb depth
        , COp3 <$> arbitrary <*> arb depth <*> arb depth <*> arb depth
        , Avg  <$> arbList depth
        , oneof [ Predict      <$> arbList depth <*> arb depth <*> arb depth
                , PredictSigma <$> arbList depth <*> arb depth <*> arb depth
                , PredictShiftMultiplier      <$> arb depth <*> arb depth
                                              <*> arb depth <*> arb depth
                , PredictSigmaShiftMultiplier <$> arb depth <*> arb depth
                                              <*> arb depth <*> arb depth
                ]
        ]

      arbList :: Integer -> Gen [CDef]
      arbList depth = take 10 <$> listOf (arb depth)

      arbData = Data <$> arbName    -- dataFilename
                     <*> arbName    -- dataDSName
                     <*> arbitrary  -- dataCF
                     <*> arbitrary  -- dataStepSizeM
                     <*> arbitrary  -- dataStartM
                     <*> arbitrary  -- dataEndM
                     <*> arbitrary  -- dataReduceM

      arbName :: Gen String
      arbName = take 200 . fromNonEmpty <$> arbitrary

  shrink cDef =
    case cDef of
      Constant n -> Constant <$> shrink n
      Data {}    -> flip mconcat cDef
                      [ const [Constant 0]
                      , shrinkLens shrName dataFilename
                      , shrinkLens shrName dataDSName
                      , shrinkLens shrink  dataCF
                      , shrinkLens shrink  dataStepSizeM
                      , shrinkLens shrink  dataStartM
                      , shrinkLens shrink  dataEndM
                      , shrinkLens shrink  dataReduceM
                      ]

      COp0 op       -> shrOp0 (COp0 op)
      COp1 op a     -> shrOp1 (COp1 op) a
      COp2 op a b   -> shrOp2 (COp2 op) a b
      COp3 op a b c -> shrOp3 (COp3 op) a b c

      Avg          xs     -> shrOp1 Avg          xs
      Predict      xs a b -> shrOp3 Predict      xs a b
      PredictSigma xs a b -> shrOp3 PredictSigma xs a b

      PredictShiftMultiplier a b c d ->
        shrOp4 PredictShiftMultiplier a b c d
      PredictSigmaShiftMultiplier a b c d ->
        shrOp4 PredictSigmaShiftMultiplier a b c d

    where
      shrinkLens :: (a -> [a]) -> (:->) f a -> f -> [f]
      shrinkLens shrinker l f = map (\a -> setL l a f) . shrinker $ getL l f

      shrName = map fromNonEmpty . shrink . NonEmpty

      shrOp0 _          =  Constant 0 : []
      shrOp1 op a       =  Constant 0
                        :  [ op a' | a' <- shrink a ]
      shrOp2 op a b     =  Constant 0
                        :  [ op a' b | a' <- shrink a ]
                        ++ [ op a b' | b' <- shrink b ]
      shrOp3 op a b c   =  Constant 0
                        :  [ op a' b c | a' <- shrink a ]
                        ++ [ op a b' c | b' <- shrink b ]
                        ++ [ op a b c' | c' <- shrink c ]
      shrOp4 op a b c d =  Constant 0
                        :  [ op a' b c d | a' <- shrink a ]
                        ++ [ op a b' c d | b' <- shrink b ]
                        ++ [ op a b c' d | c' <- shrink c ]
                        ++ [ op a b c d' | d' <- shrink d ]

instance Arbitrary CDefOp0 where
  arbitrary = elements [ Unkn, Inf, NegInf, Now ]

instance Arbitrary CDefOp1 where
  arbitrary = elements [ Un, IsInf, Sin, Cos, Log, Exp, Sqrt, Atan, Floor
                       , Ceil, Deg2Rad, Rad2Deg, Abs, Prev, Count, Time, LTime
                       ]

instance Arbitrary CDefOp2 where
  arbitrary = elements [ LT, LE, GT, GE, EQ, NE, Min, Max, Add, Sub, Mul, Div
                       , Mod, AddNAN, Atan2, Trend, TrendNAN ]

instance Arbitrary CDefOp3 where
  arbitrary = elements [ If, Limit ]

instance Arbitrary CF where
  arbitrary = elements [ CFAverage, CFMin, CFMax, CFLast ]

  shrink CFAverage = []
  shrink _         = [CFAverage]

data NumOp1 = NumOp1 String
                     (Fractional a => a -> a)
                     (CDef -> CDef)

instance Show NumOp1 where
  show (NumOp1 str _ _) = str

instance Arbitrary NumOp1 where
  arbitrary = elements [ NumOp1 "abs"    abs    cAbs
                       , NumOp1 "signum" signum cSignum
                       ]

data NumOp2 = NumOp2 String
                     (Fractional a => a -> a -> a)
                     (CDef -> CDef -> CDef)

instance Show NumOp2 where
  show (NumOp2 str _ _) = str

instance Arbitrary NumOp2 where
  arbitrary = elements [ NumOp2 "(+)" (+) cAdd
                       , NumOp2 "(*)" (*) cMul
                       , NumOp2 "(-)" (-) cSub
                       , NumOp2 "(/)" (/) cDiv
                       ]

tests_CDef :: Test
tests_CDef = $(testGroupGenerator)

prop_Num_op1_Constant :: NumOp1 -> Rational -> Bool
prop_Num_op1_Constant (NumOp1 _ opN _) num =
  opN (Constant num) == Constant (opN num)

prop_Num_op1_Other :: NumOp1 -> CDef -> Property
prop_Num_op1_Other (NumOp1 _ opN opD) cDef =
  not (isConstant cDef) ==>
    opN cDef == opD cDef

prop_Num_op2_Constant_Constant :: NumOp2 -> Rational -> Rational -> Property
prop_Num_op2_Constant_Constant (NumOp2 _ opN _) a b = b /= 0 ==>
  opN (Constant a) (Constant b) == Constant (opN a b)

prop_Num_op2_Constant_Other :: NumOp2 -> CDef -> Rational -> Property
prop_Num_op2_Constant_Other (NumOp2 _ opN opD) cDef num =
  not (isConstant cDef) ==>
    (&&) (opN cDef (Constant num) == opD cDef (Constant num))
         (opN (Constant num) cDef == opD (Constant num) cDef)

-- Helpers.

fromNonEmpty :: NonEmptyList a -> [a]
fromNonEmpty (NonEmpty xs) = xs

isConstant :: CDef -> Bool
isConstant (Constant {}) = True
isConstant _             = False
