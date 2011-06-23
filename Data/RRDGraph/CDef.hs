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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.RRDGraph.CDef
( CDefNum
, CDef (..)
, CF (..)
, StackItem (..)

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
, cUnkn
, cInf
, cNegInf
, cNow
, cPrev
, cCount
, cTime
, cLTime
, cPredict
, cPredictSigma
, cPredict'
, cPredictSigma'

, cSignum
, cDerivative
)
where

import Data.List
import Data.Record.Label
import Data.String

type CDefNum = Double

-- | A representation of DEF and CDEF.
data CDef = Constant CDefNum
          | Data { _dataFilename  :: String
                 , _dataDSName    :: String
                 , _dataCF        :: CF
                 , _dataStepSizeM :: Maybe CDefNum
                 , _dataStartM    :: Maybe CDefNum
                 , _dataEndM      :: Maybe CDefNum
                 , _dataReduceM   :: Maybe CF
                 }
          | CDefStack [CDef] [StackItem]
  deriving (Eq, Ord, Read, Show)

-- Consolidation Function
data CF = CFAverage | CFMin | CFMax | CFLast
  deriving (Eq, Ord, Read, Show)

-- | An RPN stack item.
newtype StackItem = StackItem { fromStackItem :: String }
  deriving (Eq, Ord, Read, Show, IsString)

mkLabels [''CDef]

instance Num CDef where
  (+) = cAdd
  (*) = cMul
  (-) = cSub
  abs = cAbs
  signum = cSignum
  fromInteger = Constant . fromInteger

instance Fractional CDef where
  (/) = cDiv
  fromRational = Constant . fromRational

-- Missing: SORT, REV, DUP, POP, EXC (direct stack modification)

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
cTrend    :: CDef -> CDef -> CDef
cTrendNAN :: CDef -> CDef -> CDef
cUnkn     :: CDef
cInf      :: CDef
cNegInf   :: CDef
cNow      :: CDef
cPrev     :: CDef -> CDef
cCount    :: CDef -> CDef
cTime     :: CDef -> CDef
cLTime    :: CDef -> CDef
cAvg      :: [CDef] -> CDef

cPredict       :: [CDef] -> CDef -> CDef -> CDef
cPredictSigma  :: [CDef] -> CDef -> CDef -> CDef
cPredict'      :: CDef -> CDef -> CDef -> CDef -> CDef
cPredictSigma' :: CDef -> CDef -> CDef -> CDef -> CDef

cLT       a b   = boolOp  (<)     a b   $ CDefStack [a, b]    ["LT"]
cLE       a b   = boolOp  (<=)    a b   $ CDefStack [a, b]    ["LE"]
cGT       a b   = boolOp  (>)     a b   $ CDefStack [a, b]    ["GT"]
cGE       a b   = boolOp  (>=)    a b   $ CDefStack [a, b]    ["GE"]
cEQ       a b   = boolOp  (==)    a b   $ CDefStack [a, b]    ["EQ"]
cNE       a b   = boolOp  (/=)    a b   $ CDefStack [a, b]    ["NE"]
cUn       a     =                         CDefStack [a]       ["UN"]
cIsInf    a     =                         CDefStack [a]       ["ISINF"]
cIf       a b c =                         CDefStack [a, b, c] ["IF"]
cMin      a b   = numOp2  min     a b   $ CDefStack [a, b]    ["MIN"]
cMax      a b   = numOp2  max     a b   $ CDefStack [a, b]    ["MAX"]
cLimit    a b c =                         CDefStack [a, b, c] ["LIMIT"]
cAdd      a b   = numOp2  (+)     a b   $ CDefStack [a, b]    ["+"]
cSub      a b   = numOp2  (-)     a b   $ CDefStack [a, b]    ["-"]
cMul      a b   = numOp2  (*)     a b   $ CDefStack [a, b]    ["*"]
cDiv      a b   = numOp2  (/)     a b   $ CDefStack [a, b]    ["/"]
cMod      a b   =                         CDefStack [a, b]    ["%"]
cAddNAN   a b   =                         CDefStack [a, b]    ["ADDNAN"]
cSin      a     = numOp1  sin     a     $ CDefStack [a]       ["SIN"]
cCos      a     = numOp1  cos     a     $ CDefStack [a]       ["COS"]
cLog      a     = numOp1  log     a     $ CDefStack [a]       ["LOG"]
cExp      a     = numOp1  exp     a     $ CDefStack [a]       ["EXP"]
cSqrt     a     = numOp1  sqrt    a     $ CDefStack [a]       ["SQRT"]
cAtan     a     = numOp1  atan    a     $ CDefStack [a]       ["ATAN"]
cAtan2    a b   = numOp2  atan2   a b   $ CDefStack [a, b]    ["ATAN2"]
cFloor    a     = numOp1  f       a     $ CDefStack [a]       ["FLOOR"]
  where f = fromInteger . floor
cCeil     a     = numOp1  f       a     $ CDefStack [a]       ["CEIL"]
  where f = fromInteger . ceiling
cDeg2Rad  a     = numOp1  fromDeg a     $ CDefStack [a]       ["DEG2RAD"]
  where fromDeg = (*) (pi/180)
cRad2Deg  a     = numOp1  toDeg   a     $ CDefStack [a]       ["RAD2DEG"]
  where toDeg   = (*) (180/pi)
cAbs      a     = numOp1  abs     a     $ CDefStack [a]       ["ABS"]
cTrend    a b   =                         CDefStack [a, b]    ["TREND"]
cTrendNAN a b   =                         CDefStack [a, b]    ["TRENDNAN"]
cUnkn           =                         CDefStack []        ["UNKN"]
cInf            =                         CDefStack []        ["INF"]
cNegInf         =                         CDefStack []        ["NEGINF"]
cNow            =                         CDefStack []        ["NOW"]

cAvg      xs    = CDefStack (xs ++ [genericLength xs]) ["AVG"]

-- These refer to the “current” value, which means the last one that was pushed
-- to the stack. Push a value and then immediately pop it before using these
-- instructions.
cPrev     a     = CDefStack [a] ["POP", "PREV"]
cCount    a     = CDefStack [a] ["POP", "COUNT"]
cTime     a     = CDefStack [a] ["POP", "TIME"]
cLTime    a     = CDefStack [a] ["POP", "LTIME"]

cPredict       shifts win a =
  CDefStack (shifts ++ [genericLength shifts, win, a]) ["PREDICT"]
cPredictSigma  shifts win a =
  CDefStack (shifts ++ [genericLength shifts, win, a]) ["PREDICTSIGMA"]
cPredict'      shiftMul numShifts win a =
  CDefStack [shiftMul, negate numShifts, win, a] ["PREDICT"]
cPredictSigma' shiftMul numShifts win a =
  CDefStack [shiftMul, negate numShifts, win, a] ["PREDICTSIGMA"]

cSignum :: CDef -> CDef
cSignum x = numOp1 signum x $ cIf (x `cLT` 0) (-1) $ cIf (x `cEQ` 0) 0 1

cDerivative :: CDef -> CDef
cDerivative a =
  let da = a - cPrev a
      dt = t - cPrev t
      t  = cTime a
  in  da / dt

-- Helpers.

boolOp :: (CDefNum -> CDefNum -> Bool) -> CDef -> CDef -> CDef -> CDef
boolOp op (Constant a) (Constant b) _    = Constant (if op a b then 1 else 0)
boolOp _  _            _            cDef = cDef

numOp1 :: (CDefNum -> CDefNum) -> CDef -> CDef -> CDef
numOp1 op (Constant a) _    = Constant (op a)
numOp1 _  _            cDef = cDef

numOp2 :: (CDefNum -> CDefNum -> CDefNum) -> CDef -> CDef -> CDef -> CDef
numOp2 op (Constant a) (Constant b) _    = Constant (op a b)
numOp2 _  _            _            cDef = cDef
