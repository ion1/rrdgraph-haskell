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

module Data.RRDGraph.Tests.Command
( nameIsValid
, tests_Command
)
where

import Data.RRDGraph.Command

import Control.Applicative
import Data.Char

import Test.Framework (Test)
import Test.Framework.TH (testGroupGenerator)

nameIsValid :: Name -> Bool
nameIsValid = and
            . sequence [ not . null
                       , (<= 255) . length
                       , not . all isPunctuation
                       , not . all isUpper
                       , not . all isDigit
                       ]
            . fromName

tests_Command :: Test
tests_Command = $(testGroupGenerator)
