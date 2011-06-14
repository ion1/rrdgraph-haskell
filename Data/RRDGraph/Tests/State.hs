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

import Data.RRDGraph.Command
import Data.RRDGraph.State

import Data.RRDGraph.Tests.Command

import Control.Monad
import qualified Data.Set as S

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.QuickCheck

tests_State :: Test
tests_State = $(testGroupGenerator)

prop_runGraph :: TCommand -> Bool
prop_runGraph (TCommand cmd) =
  runGraph (addCommand cmd) == [formatCommand cmd]

prop_runGraphRaw :: TCommand -> Bool
prop_runGraphRaw (TCommand cmd) =
  runGraphRaw (addCommand cmd) == [cmd]

prop_namesAreValid :: NonNegative Int -> Property
prop_namesAreValid (NonNegative n) =
  let n'    = min n 100
      names = evalGraphState (replicateM n' newName)
  in  printTestCase ("Got names: " ++ show (map fromName names))
        $ all nameIsValid names

prop_namesAreUnique :: NonNegative Int -> Property
prop_namesAreUnique (NonNegative n) =
  let n'    = min n 100
      names = evalGraphState (replicateM n' newName)
  in  printTestCase ("Got names: " ++ show (map fromName names))
        $ (S.size . S.fromList) names == n'

prop_addCommand :: [TCommand] -> Bool
prop_addCommand cmds =
  let cmds' = map fromTCommand . take 3 $ cmds
  in  runGraphRaw (mapM_ addCommand cmds') == cmds'
