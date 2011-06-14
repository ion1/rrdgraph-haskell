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

module Data.RRDGraph.Tests.State (tests_State)
where

import Data.RRDGraph.State

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.QuickCheck (NonNegative (..))

tests_State :: Test
tests_State = $(testGroupGenerator)

prop_namesAreValid :: NonNegative Int -> Bool
prop_namesAreValid (NonNegative n) =
  let n' = min n 100
  in  all nameIsValid $ evalGraphState (replicateM n' newName)

prop_namesAreUnique :: NonNegative Int -> Bool
prop_namesAreUnique (NonNegative n) =
  let n' = min n 100
  in  (== n') . length . nub $ evalGraphState (replicateM n' newName)

nameIsValid :: Name -> Bool
nameIsValid (Name str) =
  liftA2 (&&) (not . all isUpper) (not . all isDigit) str
