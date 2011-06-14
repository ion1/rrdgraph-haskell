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

{-# LANGUAGE TemplateHaskell #-}

module Data.RRDGraph.CDef
( CDef (..)

, CDefOp0 (..)
, CDefOp1 (..)
, CDefOp2 (..)
, CDefOp3 (..)

, CF (..)

, dataFilename
, dataDSName
, dataCF
, dataStepSizeM
, dataStartM
, dataEndM
, dataReduceM

, cLT
, cLE
, cGT
, cGE
, cEQ
, cNE
, cUn
, cIsInf
, cIf
, cMin
, cMax
, cLimit
, cAdd
, cSub
, cMul
, cDiv
, cMod
, cAddNAN
, cSin
, cCos
, cLog
, cExp
, cSqrt
, cAtan
, cAtan2
, cFloor
, cCeil
, cDeg2Rad
, cRad2Deg
, cAbs
, cAvg
, cTrend
, cTrendNAN
, cPredict
, cPredictSigma
, cPredictShiftMultiplier
, cPredictSigmaShiftMultiplier
, cUnkn
, cInf
, cNegInf
, cPrev
, cCount
, cNow
, cTime
, cLTime
, cSignum
, cDerivative
)
where

import Prelude hiding (LT, EQ, GT)

import Data.Record.Label

-- Missing: SORT, REV, DUP, POP, EXC (direct stack modification)

data CDef = Constant Rational
          | Data { _dataFilename  :: String
                 , _dataDSName    :: String
                 , _dataCF        :: CF
                 , _dataStepSizeM :: Maybe Rational
                 , _dataStartM    :: Maybe Rational
                 , _dataEndM      :: Maybe Rational
                 , _dataReduceM   :: Maybe CF
                 }
          | COp0 CDefOp0
          | COp1 CDefOp1 CDef
          | COp2 CDefOp2 CDef CDef
          | COp3 CDefOp3 CDef CDef CDef
          | Avg [CDef]
          | Predict [CDef] CDef CDef
          | PredictSigma [CDef] CDef CDef
          | PredictShiftMultiplier CDef CDef CDef CDef
          | PredictSigmaShiftMultiplier CDef CDef CDef CDef
  deriving (Eq, Read, Show)

instance Num CDef where
  (+) = defOper2 (+) cAdd
  (*) = defOper2 (*) cMul
  (-) = defOper2 (-) cSub
  abs = defOper1 abs cAbs
  signum = defOper1 signum cSignum
  fromInteger = Constant . fromInteger

instance Fractional CDef where
  (/) = defOper2 (/) cDiv
  fromRational = Constant . fromRational

data CDefOp0 = Unkn | Inf | NegInf | Now
  deriving (Eq, Read, Show)

data CDefOp1 = Un | IsInf | Sin | Cos | Log | Exp | Sqrt | Atan | Floor | Ceil
             | Deg2Rad | Rad2Deg | Abs | Prev | Count | Time | LTime
  deriving (Eq, Read, Show)

data CDefOp2 = LT | LE | GT | GE | EQ | NE | Min | Max | Add | Sub | Mul | Div
             | Mod | AddNAN | Atan2 | Trend | TrendNAN
  deriving (Eq, Read, Show)

data CDefOp3 = If | Limit
  deriving (Eq, Read, Show)


cLT       :: CDef -> CDef -> CDef
cLE       :: CDef -> CDef -> CDef
cGT       :: CDef -> CDef -> CDef
cGE       :: CDef -> CDef -> CDef
cEQ       :: CDef -> CDef -> CDef
cNE       :: CDef -> CDef -> CDef
cUn       :: CDef -> CDef
cIsInf    :: CDef -> CDef
cIf       :: CDef -> CDef -> CDef -> CDef
cMin      :: CDef -> CDef -> CDef
cMax      :: CDef -> CDef -> CDef
cLimit    :: CDef -> CDef -> CDef -> CDef
cAdd      :: CDef -> CDef -> CDef
cSub      :: CDef -> CDef -> CDef
cMul      :: CDef -> CDef -> CDef
cDiv      :: CDef -> CDef -> CDef
cMod      :: CDef -> CDef -> CDef
cAddNAN   :: CDef -> CDef -> CDef
cSin      :: CDef -> CDef
cCos      :: CDef -> CDef
cLog      :: CDef -> CDef
cExp      :: CDef -> CDef
cSqrt     :: CDef -> CDef
cAtan     :: CDef -> CDef
cAtan2    :: CDef -> CDef -> CDef
cFloor    :: CDef -> CDef
cCeil     :: CDef -> CDef
cDeg2Rad  :: CDef -> CDef
cRad2Deg  :: CDef -> CDef
cAbs      :: CDef -> CDef
cAvg      :: [CDef] -> CDef
cTrend    :: CDef -> CDef -> CDef
cTrendNAN :: CDef -> CDef -> CDef

cPredict                     :: [CDef] -> CDef -> CDef -> CDef
cPredictSigma                :: [CDef] -> CDef -> CDef -> CDef
cPredictShiftMultiplier      :: CDef -> CDef -> CDef -> CDef -> CDef
cPredictSigmaShiftMultiplier :: CDef -> CDef -> CDef -> CDef -> CDef

cUnkn   :: CDef
cInf    :: CDef
cNegInf :: CDef
cPrev   :: CDef -> CDef
cCount  :: CDef -> CDef
cNow    :: CDef
cTime   :: CDef -> CDef
cLTime  :: CDef -> CDef

cLT       = COp2 LT
cLE       = COp2 LE
cGT       = COp2 GT
cGE       = COp2 GE
cEQ       = COp2 EQ
cNE       = COp2 NE
cUn       = COp1 Un
cIsInf    = COp1 IsInf
cIf       = COp3 If
cMin      = COp2 Min
cMax      = COp2 Max
cLimit    = COp3 Limit
cAdd      = COp2 Add
cSub      = COp2 Sub
cMul      = COp2 Mul
cDiv      = COp2 Div
cMod      = COp2 Mod
cAddNAN   = COp2 AddNAN
cSin      = COp1 Sin
cCos      = COp1 Cos
cLog      = COp1 Log
cExp      = COp1 Exp
cSqrt     = COp1 Sqrt
cAtan     = COp1 Atan
cAtan2    = COp2 Atan2
cFloor    = COp1 Floor
cCeil     = COp1 Ceil
cDeg2Rad  = COp1 Deg2Rad
cRad2Deg  = COp1 Rad2Deg
cAbs      = COp1 Abs
cAvg      = Avg
cTrend    = COp2 Trend
cTrendNAN = COp2 TrendNAN

cPredict                     = Predict
cPredictSigma                = PredictSigma
cPredictShiftMultiplier      = PredictShiftMultiplier
cPredictSigmaShiftMultiplier = PredictSigmaShiftMultiplier

cUnkn   = COp0 Unkn
cInf    = COp0 Inf
cNegInf = COp0 NegInf
cPrev   = COp1 Prev
cCount  = COp1 Count
cNow    = COp0 Now
cTime   = COp1 Time
cLTime  = COp1 LTime

cSignum :: CDef -> CDef
cSignum x = cIf (x `cLT` 0) (-1) $ cIf (x `cEQ` 0) 0 1

cDerivative :: CDef -> CDef
cDerivative a =
  let da = a - cPrev a
      dt = t - cPrev t
      t  = cTime a
  in  da / dt

-- Helpers for the Num CDef instance definitions.

defOper1 :: (Rational -> Rational)
         -> (CDef -> CDef)
         -> CDef -> CDef
defOper1 opN _   (Constant a) = Constant (opN a)
defOper1 _   opD a            = opD a

defOper2 :: (Rational -> Rational -> Rational)
         -> (CDef -> CDef -> CDef)
         -> CDef -> CDef -> CDef
defOper2 opN _   (Constant a) (Constant b) = Constant (opN a b)
defOper2 _   opD a            b            = opD a b

-- Consolidation Function
data CF = CFAverage | CFMin | CFMax | CFLast
  deriving (Eq, Read, Show)

mkLabels [''CDef]
