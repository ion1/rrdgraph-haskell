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

module Main where

import Data.RRDGraph.Tests.CDef (tests_CDef)
--import Data.RRDGraph.Tests.Command (tests_Command)
--import Data.RRDGraph.Tests.Fields (tests_Fields)
import Data.RRDGraph.Tests.Internal (tests_Internal)
--import Data.RRDGraph.Tests.State (tests_State)
import Data.RRDGraph.Tests.VDef (tests_VDef)

import Test.Framework (defaultMain)

main :: IO ()
main = defaultMain [ tests_CDef
                   -- , tests_Command
                   -- , tests_Fields
                   , tests_Internal
                   -- , tests_State
                   , tests_VDef
                   ]
