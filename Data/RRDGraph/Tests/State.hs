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
import Data.Function
import Data.Record.Label
import qualified Data.Set as S

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.QuickCheck

tests_State :: Test
tests_State = $(testGroupGenerator)

prop_runGraph :: Command -> Bool
prop_runGraph cmd =
  runGraph (addCommand cmd) == [formatCommand cmd]

prop_runGraphRaw :: Command -> Bool
prop_runGraphRaw cmd =
  runGraphRaw (addCommand cmd) == [cmd]

prop_newName_valid :: NonNegative Int -> Property
prop_newName_valid (NonNegative n) =
  let n'    = min n 100
      names = evalGraphState (replicateM n' newName)
  in  printNames names $ all nameIsValid names

prop_newName_unique :: NonNegative Int -> Property
prop_newName_unique (NonNegative n) =
  let n'    = min n 100
      names = evalGraphState (replicateM n' newName)
  in  printNames names $ numUniques names == n'

prop_addCommand :: [Command] -> Bool
prop_addCommand cmds =
  let cmds' = take 5 $ cmds
  in  runGraphRaw (mapM_ addCommand cmds') == cmds'

prop_addCommandDef_duplicates :: [Command] -> Property
prop_addCommandDef_duplicates cmds =
  let cmds'  = take 5 . filter applies_addCommandDef $ cmds
      cmds'' = cmds' ++ cmds'
      names  = evalGraphState (mapM addCommandDef cmds'')
  in  printNames names $
        numUniques names == (numUniques . map commandNullDefines) cmds''

prop_addCommandDef_commands :: [Command] -> Property
prop_addCommandDef_commands cmds =
  let cmds'   = take 5 . filter applies_addCommandDef $ cmds
      cmds''  = cmds' ++ cmds'
      cmdsRes = runGraphRaw (mapM_ addCommandDef cmds'')
  in  printGot "cmdsRes" cmdsRes $
        ((==) `on` S.fromList . map commandNullDefines) cmds'' cmdsRes

applies_addCommandDef :: Command -> Bool
applies_addCommandDef (DataCommand {})  = True
applies_addCommandDef (CDefCommand {})  = True
applies_addCommandDef (VDefCommand {})  = True
applies_addCommandDef (GraphCommand {}) = False

-- Helpers.

numUniques :: Ord a => [a] -> Int
numUniques = S.size . S.fromList

commandNullDefines :: Command -> Command
commandNullDefines = setL cmdDefines (Name "")

printGot :: (Show a, Testable prop) => String -> a -> prop -> Property
printGot name value = printTestCase ("Got " ++ name ++ ": " ++ show value)

printNames :: Testable prop => [Name] -> prop -> Property
printNames = printGot "names" . map fromName
