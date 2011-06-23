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

{-# LANGUAGE OverloadedStrings #-}

module Data.RRDGraph.VDef
( VDef (..)
, vMaximum
, vMinimum
, vAverage
, vStDev
, vLast
, vFirst
, vTotal
, vLslSlope
, vLslInt
, vLslCorrel
, vPercent
, vPercentNAN
)
where

import Data.RRDGraph.CDef
import Data.RRDGraph.Internal

-- | A representation of VDEF.
data VDef = VDefStack [CDef] [StackItem]
  deriving (Eq, Ord, Read, Show)

vMaximum   :: CDef -> VDef
vMinimum   :: CDef -> VDef
vAverage   :: CDef -> VDef
vStDev     :: CDef -> VDef
vLast      :: CDef -> VDef
vFirst     :: CDef -> VDef
vTotal     :: CDef -> VDef
vLslSlope  :: CDef -> VDef
vLslInt    :: CDef -> VDef
vLslCorrel :: CDef -> VDef

vPercent    :: CDef -> CDefNum -> VDef
vPercentNAN :: CDef -> CDefNum -> VDef

vMaximum   a = VDefStack [a] ["MAXIMUM"]
vMinimum   a = VDefStack [a] ["MINIMUM"]
vAverage   a = VDefStack [a] ["AVERAGE"]
vStDev     a = VDefStack [a] ["STDEV"]
vLast      a = VDefStack [a] ["LAST"]
vFirst     a = VDefStack [a] ["FIRST"]
vTotal     a = VDefStack [a] ["TOTAL"]
vLslSlope  a = VDefStack [a] ["LSLSLOPE"]
vLslInt    a = VDefStack [a] ["LSLINT"]
vLslCorrel a = VDefStack [a] ["LSLCORREL"]

vPercent    a n = VDefStack [a] [StackItem . numericField $ n, "PERCENT"]
vPercentNAN a n = VDefStack [a] [StackItem . numericField $ n, "PERCENTNAN"]
