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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Data.RRDGraph.Tests.CDef (tests_CDef)
where

import Data.RRDGraph.CDef
import Data.RRDGraph.Internal

import Control.Applicative
import Data.List
import Data.Monoid

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.QuickCheck

instance Arbitrary CDef where
  arbitrary = sized $ \(fromIntegral -> n) -> arb (min n 3)
    where
      arb :: Integer -> Gen CDef
      arb (pred -> depth) | depth >= 0 =
        frequency [ (2, arbConstant), (1, arbStack depth) ]
      arb _ = arbConstant

      arbConstant = frequency [ (2, liftA Constant arbitrary), (1, arbData) ]

      arbData = Data <$> arbName    -- dataFilename
                     <*> arbName    -- dataDSName
                     <*> arbitrary  -- dataCF
                     <*> arbitrary  -- dataStepSizeM
                     <*> arbitrary  -- dataStartM
                     <*> arbitrary  -- dataEndM
                     <*> arbitrary  -- dataReduceM

      arbStack depth = CDefStack <$> arbCDefs depth <*> arbStackItems

      arbCDefs :: Integer -> Gen [CDef]
      arbCDefs depth = take 10 <$> listOf (arb depth)

      arbStackItems :: Gen [StackItem]
      arbStackItems = take 5 . fromNonEmpty <$> arbitrary

      arbName :: Gen String
      arbName = take 10 . fromNonEmpty <$> arbitrary

  shrink cDef =
    case cDef of
      Constant n -> Constant <$> shrink n
      Data {}    -> (`mconcat` cDef)
                      [ const [Constant 0]
                      , shrinkLens shrName dataFilename
                      , shrinkLens shrName dataDSName
                      , shrinkLens shrink  dataCF
                      , shrinkLens shrink  dataStepSizeM
                      , shrinkLens shrink  dataStartM
                      , shrinkLens shrink  dataEndM
                      , shrinkLens shrink  dataReduceM
                      ]
      CDefStack cDefs stack ->
        concat [ [Constant 0]
               , liftA2 CDefStack (shrink cDefs) [stack]
               , liftA2 CDefStack [cDefs] (shrStack stack)
               ]
    where
      shrName  = map fromNonEmpty . shrink . NonEmpty
      shrStack = wrapShrink NonEmpty fromNonEmpty shrink

instance Arbitrary CF where
  arbitrary = elements [ CFAverage, CFMin, CFMax, CFLast ]

  shrink CFAverage = []
  shrink _         = [CFAverage]

instance Arbitrary StackItem where
  arbitrary = StackItem . take 10 . fromNonEmpty <$> arbitrary
  shrink = wrapShrink fromStackItem StackItem
         . wrapShrink NonEmpty      fromNonEmpty
         $ shrink

data NumOp1 = NumOp1 String (Fractional a => a -> a)
data NumOp2 = NumOp2 String (Fractional a => a -> a -> a)

tests_CDef :: Test
tests_CDef = $(testGroupGenerator)

prop_stack_op0 :: Property
prop_stack_op1 :: CDef -> Property
prop_stack_op2 :: CDef -> CDef -> Property
prop_stack_op3 :: CDef -> CDef -> CDef -> Property

prop_stack_op0       = stackProp ops0 []        id
prop_stack_op1 a     = stackProp ops1 [a]       $ \f -> f a
prop_stack_op2 a b   = stackProp ops2 [a, b]    $ \f -> f a b
prop_stack_op3 a b c = stackProp ops3 [a, b, c] $ \f -> f a b c

ops0 :: [(String, CDef, [StackItem])]
ops0 = [ ("cUnkn",   cUnkn,   ["UNKN"])
       , ("cInf",    cInf,    ["INF"])
       , ("cNegInf", cNegInf, ["NEGINF"])
       , ("cNow",    cNow,    ["NOW"])
       ]

ops1 :: [(String, CDef -> CDef, [StackItem])]
ops1 = [ ("cUn",      cUn,      ["UN"])
       , ("cIsInf",   cIsInf,   ["ISINF"])
       , ("cSin",     cSin,     ["SIN"])
       , ("cCos",     cCos,     ["COS"])
       , ("cLog",     cLog,     ["LOG"])
       , ("cExp",     cExp,     ["EXP"])
       , ("cSqrt",    cSqrt,    ["SQRT"])
       , ("cAtan",    cAtan,    ["ATAN"])
       , ("cFloor",   cFloor,   ["FLOOR"])
       , ("cCeil",    cCeil,    ["CEIL"])
       , ("cDeg2Rad", cDeg2Rad, ["DEG2RAD"])
       , ("cRad2Deg", cRad2Deg, ["RAD2DEG"])
       , ("cAbs",     cAbs,     ["ABS"])
       , ("cPrev",    cPrev,    ["POP", "PREV"])
       , ("cCount",   cCount,   ["POP", "COUNT"])
       , ("cTime",    cTime,    ["POP", "TIME"])
       , ("cLTime",   cLTime,   ["POP", "LTIME"])
       ]

ops2 :: [(String, CDef -> CDef -> CDef, [StackItem])]
ops2 = [ ("cLT",       cLT,       ["LT"])
       , ("cLE",       cLE,       ["LE"])
       , ("cGT",       cGT,       ["GT"])
       , ("cGE",       cGE,       ["GE"])
       , ("cEQ",       cEQ,       ["EQ"])
       , ("cNE",       cNE,       ["NE"])
       , ("cMin",      cMin,      ["MIN"])
       , ("cMax",      cMax,      ["MAX"])
       , ("cAdd",      cAdd,      ["+"])
       , ("cSub",      cSub,      ["-"])
       , ("cMul",      cMul,      ["*"])
       , ("cDiv",      cDiv,      ["/"])
       , ("cMod",      cMod,      ["%"])
       , ("cAddNAN",   cAddNAN,   ["ADDNAN"])
       , ("cAtan2",    cAtan2,    ["ATAN2"])
       , ("cTrend",    cTrend,    ["TREND"])
       , ("cTrendNAN", cTrendNAN, ["TRENDNAN"])
       ]

ops3 :: [(String, CDef -> CDef -> CDef -> CDef, [StackItem])]
ops3 = [ ("cIf",    cIf,    ["IF"])
       , ("cLimit", cLimit, ["LIMIT"])
       ]

prop_stack_avg :: [CDef] -> Property
prop_stack_avg (take 10 -> xs) =
  stackProp [ ("cAvg", cAvg, ["AVG"]) ]
            (xs ++ [genericLength xs])
            $ \f -> f xs

prop_stack_predict :: [CDef] -> CDef -> CDef -> Property
prop_stack_predict (take 10 -> shifts) window a =
  stackProp [ ("cPredict",      cPredict,      ["PREDICT"])
            , ("cPredictSigma", cPredictSigma, ["PREDICTSIGMA"])
            ]
            (shifts ++ [genericLength shifts, window, a])
            $ \f -> f shifts window a

prop_stack_predict' :: CDef -> CDef -> CDef -> CDef -> Property
prop_stack_predict' shiftMul numShifts window a =
  stackProp [ ("cPredict'",      cPredict',      ["PREDICT"])
            , ("cPredictSigma'", cPredictSigma', ["PREDICTSIGMA"])
            ]
            [shiftMul, negate numShifts, window, a]
            $ \f -> f shiftMul numShifts window a

stackProp :: [(String, f, [StackItem])] -> [CDef] -> (f -> CDef) -> Property
stackProp ops cDefs opF =
  conjoin . flip map ops $ \(name, op, stack) ->
    let cDef = opF op
    in  not (isConstant cDef) ==>
          assert (name ++ ": ") (==) (CDefStack cDefs stack) cDef

-- Constant (abs 42) == abs (Constant 42).
prop_Num_op1 :: CDefNum -> Property
prop_Num_op1 a =
  conjoin . flip map numInstanceOps1 $ \(NumOp1 name op) ->
    isFinite (op a) ==>
      assert (name ++ ": ") (==) (Constant (op a)) (op (Constant a))

numInstanceOps1 :: [NumOp1]
numInstanceOps1 = [ NumOp1 "abs"    abs
                  , NumOp1 "signum" signum
                  ]

-- Constant 42 + Constant 43 == Constant (42 + 43).
prop_Num_op2 :: CDefNum -> CDefNum -> Property
prop_Num_op2 a b =
  conjoin . flip map numInstanceOps2 $ \(NumOp2 name op) ->
    isFinite (op a b) ==>
      assert (name ++ ": ") (==) (Constant (op a b))
                                 (op (Constant a) (Constant b))

numInstanceOps2 :: [NumOp2]
numInstanceOps2 = [ NumOp2 "(+)" (+)
                  , NumOp2 "(*)" (*)
                  , NumOp2 "(-)" (-)
                  , NumOp2 "(/)" (/)
                  ]

-- cIsInf 42 isn’t changed to Constant n.
prop_noOptimization_op1 :: CDef -> Property
prop_noOptimization_op1 cDef =
  conjoin . flip map nonOptimizedOps1 $ \(name, op) ->
    assertIsStack (name ++ " " ++ show cDef) (op cDef)

nonOptimizedOps1 :: [(String, CDef -> CDef)]
nonOptimizedOps1 = [ ("Un",    cUn)
                   , ("IsInf", cIsInf)
                   , ("Prev",  cPrev)
                   , ("Count", cCount)
                   , ("Time",  cTime)
                   , ("LTime", cLTime)
                   ]

-- 42 `cTrend` 43 isn’t changed to Constant n.
prop_noOptimization_op2 :: CDef -> CDef -> Property
prop_noOptimization_op2 cDefA cDefB =
  conjoin . flip map nonOptimizedOps2 $ \(name, op) ->
    assertIsStack (name ++ " " ++ show cDefA ++ " " ++ show cDefB)
                  (op cDefA cDefB)

nonOptimizedOps2 :: [(String, CDef -> CDef -> CDef)]
nonOptimizedOps2 = [ ("Mod",      cMod)
                   , ("AddNAN",   cAddNAN)
                   , ("Trend",    cTrend)
                   , ("TrendNAN", cTrendNAN)
                   ]

-- cIf a b c isn’t changed to Constant n.
prop_noOptimization_op3 :: CDef -> CDef -> CDef -> Property
prop_noOptimization_op3 cDefA cDefB cDefC =
  conjoin . flip map nonOptimizedOps3 $ \(name, op) ->
    assertIsStack (name ++ " " ++ show cDefA ++ " " ++ show cDefB
                         ++ " " ++ show cDefC)
                  (op cDefA cDefB cDefC)

nonOptimizedOps3 :: [(String, CDef -> CDef -> CDef -> CDef)]
nonOptimizedOps3 = [ ("If",    cIf)
                   , ("Limit", cLimit)
                   ]

-- cSin 42 is changed to Constant (sin 42).
prop_optimization_op1_Constant :: CDefNum -> Property
prop_optimization_op1_Constant a =
  conjoin . flip map optimizedOps1 $ \(name, opN, opD) ->
    isFinite (opN a) ==>
      assert (name ++ ": ") (==) (Constant (opN a)) (opD (Constant a))

-- cSin nonConstant isn’t changed to Constant n.
prop_optimization_op1_Other :: CDef -> Property
prop_optimization_op1_Other cDef = not (isConstant cDef) ==>
  conjoin . flip map optimizedOps1 $ \(name, _opN, opD) ->
    assertIsStack (name ++ " " ++ show cDef) (opD cDef)

optimizedOps1 :: [(String, CDefNum -> CDefNum, CDef -> CDef)]
optimizedOps1 = [ ("Sin",     sin,         cSin)
                , ("Cos",     cos,         cCos)
                , ("Log",     log,         cLog)
                , ("Exp",     exp,         cExp)
                , ("Sqrt",    sqrt,        cSqrt)
                , ("Atan",    atan,        cAtan)
                , ("Floor",   iOp floor,   cFloor)
                , ("Ceil",    iOp ceiling, cCeil)
                , ("Deg2Rad", fromDeg,     cDeg2Rad)
                , ("Rad2Deg", toDeg,       cRad2Deg)
                , ("Abs",     abs,         cAbs)
                , ("Signum",  signum,      cSignum)
                ]
  where
    iOp f = fromInteger . f
    fromDeg = (*) (pi/180)
    toDeg   = (*) (180/pi)

-- cMin 42 43 is changed to Constant 42.
prop_optimization_op2_Constant :: CDefNum -> CDefNum -> Property
prop_optimization_op2_Constant a b =
  conjoin . flip map optimizedOps2 $ \(name, opN, opD) ->
    isFinite (opN a b) ==>
      assert (name ++ ": ") (==) (Constant (opN a b))
                                 (opD (Constant a) (Constant b))

-- cMin 42 nonConstant isn’t changed to Constant n.
prop_optimization_op2_Other :: CDefNum -> CDef -> Property
prop_optimization_op2_Other a cDef = not (isConstant cDef) ==>
  conjoin . flip map optimizedOps2 $ \(name, _opN, opD) ->
    conjoin
      [ assertIsStack (name ++ " " ++ show a ++ " " ++ show cDef)
                      (opD (Constant a) cDef)
      , assertIsStack (name ++ " " ++ show cDef ++ " " ++ show a)
                      (opD cDef (Constant a))
      ]

optimizedOps2 :: [( String, CDefNum -> CDefNum -> CDefNum
                  , CDef -> CDef -> CDef )]
optimizedOps2 = [ ("LT",     bOp (<),  cLT)
                , ("LE",     bOp (<=), cLE)
                , ("GT",     bOp (>),  cGT)
                , ("GE",     bOp (>=), cGE)
                , ("EQ",     bOp (==), cEQ)
                , ("NE",     bOp (/=), cNE)
                , ("Min",    min,      cMin)
                , ("Max",    max,      cMax)
                , ("Add",    (+),      cAdd)
                , ("Sub",    (-),      cSub)
                , ("Mul",    (*),      cMul)
                , ("Div",    (/),      cDiv)
                , ("Atan2",  atan2,    cAtan2)
                ]
  where
    bOp f = (fromIntegral . fromEnum) .: f

-- Helpers.

assert :: Show a => String -> (a -> a -> Bool) -> a -> a -> Property
assert prefix op expected actual =
  let message = prefix ++ "Expected " ++ show expected
                       ++ ", got "    ++ show actual
  in  printTestCase message (expected `op` actual)

assertIsStack :: String -> CDef -> Property
assertIsStack expected actual =
  let message =  "Expected " ++ expected ++ " to be a CDefStack, got "
              ++ show actual
  in  printTestCase message (isStack actual)

fromNonEmpty :: NonEmptyList a -> [a]
fromNonEmpty (NonEmpty xs) = xs

isConstant :: CDef -> Bool
isConstant (Constant {}) = True
isConstant _             = False

isStack :: CDef -> Bool
isStack (CDefStack {}) = True
isStack _              = False

isFinite :: RealFloat a => a -> Bool
isFinite = not . liftA2 (||) isNaN isInfinite
