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

module Data.RRDGraph.Tests.Internal (tests_Internal)
where

import Data.RRDGraph.Fields
import Data.RRDGraph.Internal

import Control.Applicative
import Data.List
import Data.Monoid
import Data.Record.Label

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.QuickCheck

data Foo = Foo { _fooFoo  :: String
               , _fooBarM :: Maybe String
               }
  deriving (Eq, Ord, Read, Show)

mkLabels [''Foo]

instance Arbitrary Foo where
  arbitrary = Foo <$> arbitrary <*> arbitrary
  shrink = mconcat [ shrinkLens shrink fooFoo
                   , shrinkLens shrink fooBarM ]

tests_Internal :: Test
tests_Internal = $(testGroupGenerator)

prop_fLens :: Foo -> Bool
prop_fLens f = runField (fLens fooFoo) f == Just (getL fooFoo f)

prop_fLensM :: Foo -> Bool
prop_fLensM f = runField (fLensM fooBarM) f == getL fooBarM f

prop_shrinkLens :: Foo -> Bool
prop_shrinkLens f@(Foo foo bar) =
  shrinkLens shrink fooBarM f == [ Foo foo bar' | bar' <- shrink bar ]

prop_wrapShrink :: [Integer] -> Bool
prop_wrapShrink =
  liftA2 (==) (wrapShrink NonEmpty (\(NonEmpty xs) -> xs) shrink)
              (map (\(NonEmpty xs) -> xs) . shrink . NonEmpty)

prop_numericField :: Rational -> Bool
prop_numericField n =
  case (reads :: ReadS Double) (numericField n) of
    (n',""):_ -> (realToFrac n) == n'
    _         -> False

prop_numUniques :: [String] -> Bool
prop_numUniques xs = (length . nub) xs == numUniques xs
