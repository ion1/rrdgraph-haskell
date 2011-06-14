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

module Data.RRDGraph.Tests.Fields (tests_Fields)
where

import Data.RRDGraph.Fields

import Control.Applicative
import Control.Monad.Reader
import Data.Monoid

import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.HUnit (Assertion, (@?=))

tests_Fields :: Test
tests_Fields = $(testGroupGenerator)

prop_pure :: Integer -> Bool
prop_pure a = runField (pure a) () == Just a

prop_asks :: Integer -> Bool
prop_asks a = runField (asks id) a == Just a

prop_asksM :: Maybe Integer -> Bool
prop_asksM a = runField (asksM id) a == a

case_mempty :: Assertion
case_mempty = runField (mempty :: Field a String) () @?= Nothing

prop_mappendMempty :: String -> Bool
prop_mappendMempty a =
  runField (mempty `mappend` pure a) () == Just a

prop_mappendStrings :: String -> String -> Bool
prop_mappendStrings a b =
  runField (pure a `mappend` pure b) () == Just (a ++ b)

prop_fmap :: Maybe Integer -> Bool
prop_fmap a = runField (fmap (42-) (asksM id)) a == fmap (42-) a

prop_appl :: Maybe Integer -> Maybe Integer -> Bool
prop_appl a b =
  runField (liftA2 (-) (asksM fst) (asksM snd)) (a,b) == liftA2 (-) a b

prop_bind :: Maybe Integer -> Maybe Integer -> Bool
prop_bind a b =
  runField (liftM2 (-) (asksM fst) (asksM snd)) (a,b) == liftA2 (-) a b

prop_runFields :: [Maybe Integer] -> Bool
prop_runFields as =
  let fs  = map (liftA2 (-) ask . field . const) as
      as' = map (liftA (42-)) as
  in  runFields fs 42 == as'
