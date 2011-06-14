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

-- The only orphan instances are for the module being tested.
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Data.RRDGraph.Tests.Command
( NameChar (..)
, nameIsValid
, tests_Command
)
where

import Data.RRDGraph.Command
import Data.RRDGraph.Fields
import Data.RRDGraph.Internal

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Record.Label
import qualified Data.Set as S

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.QuickCheck

instance Arbitrary Command where
  arbitrary = oneof [ arbDataCommand
                    , arbDefCommand CDefCommand
                    , arbDefCommand VDefCommand
                    , arbGraphCommand
                    ]
    where
      arbDataCommand       = liftA2 DataCommand arbDefines arbText
      arbDefCommand constr = liftA3 constr arbDefines arbStack arbReferences
      arbGraphCommand      = liftA2 GraphCommand arbText arbReferences

      arbDefines :: Gen Name
      arbDefines = arbitrary

      arbText :: Gen String
      arbText = take 1000 . fromNonEmpty <$> arbitrary

      arbStack :: Gen [StackItem]
      arbStack  =  take 20 . map (StackItem . fromName)
                .  fromNonEmpty
               <$> arbitrary

      arbReferences :: Gen (S.Set Name)
      arbReferences = S.fromList . take 20 <$> arbitrary

  shrink cmd =
    case cmd of
      DataCommand {} ->
        flip mconcat cmd [ shrinkLens shrDefines cmdDefines
                         , shrinkLens shrText    cmdText    ]
      CDefCommand {} ->
        flip mconcat cmd [ shrinkLens shrDefines    cmdDefines
                         , shrinkLens shrStack      cmdStack
                         , shrinkLens shrReferences cmdReferences ]
      VDefCommand {} ->
        flip mconcat cmd [ shrinkLens shrDefines    cmdDefines
                         , shrinkLens shrStack      cmdStack
                         , shrinkLens shrReferences cmdReferences ]
      GraphCommand {} ->
        flip mconcat cmd [ shrinkLens shrText       cmdText
                         , shrinkLens shrReferences cmdReferences ]
    where
      shrDefines :: Name -> [Name]
      shrDefines = shrink

      shrText :: String -> [String]
      shrText = wrapShrink NonEmpty fromNonEmpty shrink

      shrStack :: [StackItem] -> [[StackItem]]
      shrStack = wrapShrink (map fromStackItem) (map StackItem)
               . wrapShrink (map Name)          (map fromName)
               . wrapShrink NonEmpty            fromNonEmpty
               $ shrink

      shrReferences :: S.Set Name -> [S.Set Name]
      shrReferences = wrapShrink S.toList S.fromList shrink

instance Arbitrary Name where
  arbitrary = (Name . map fromNameChar . take 255 <$> listOf1 arbitrary)
      `suchThat` nameIsValid

  shrink = filter nameIsValid
         . ( wrapShrink fromName       Name
           . wrapShrink (map NameChar) (map fromNameChar)
           $ shrink )

newtype NameChar = NameChar { fromNameChar :: Char }
  deriving (Eq, Ord, Read, Show)

instance Arbitrary NameChar where
  arbitrary = NameChar <$> elements chars
    where chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_"

  shrink (NameChar 'a') = []
  shrink _              = [NameChar 'a']

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

prop_formatCommand :: Command -> Bool
prop_formatCommand c =
  formatCommand c == case c of
    DataCommand {} ->
      concat . catMaybes . flip runFields c $
        [ "DEF:", fromName <$> fLens cmdDefines, "=", fLens cmdText ]

    CDefCommand {}  -> expectedDefCommand "CDEF" c
    VDefCommand {}  -> expectedDefCommand "VDEF" c
    GraphCommand {} -> getL cmdText c

  where
    expectedDefCommand prefix =
      concat . catMaybes . runFields
        [ prefix, ":", fromName <$> fLens cmdDefines, "="
        , intercalate "," . map fromStackItem <$> fLens cmdStack ]

-- Helpers.

fromNonEmpty :: NonEmptyList a -> [a]
fromNonEmpty (NonEmpty xs) = xs
