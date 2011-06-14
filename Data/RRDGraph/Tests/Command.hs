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

{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Data.RRDGraph.Tests.Command
( TCommand (..)
, TName (..)
, TNameChar (..)
, nameIsValid
, tests_Command
)
where

import Data.RRDGraph.Command
import Data.RRDGraph.Fields

import Control.Applicative
import Control.Monad.Reader
import Data.Char
import Data.List
import Data.Maybe
import Data.Record.Label
import qualified Data.Set as S

import Test.Framework (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.QuickCheck

newtype TCommand = TCommand { fromTCommand :: Command }
  deriving (Eq, Ord, Read, Show)

instance Arbitrary TCommand where
  arbitrary = TCommand <$> oneof [ arbDataCommand
                                 , arbDefCommand CDefCommand
                                 , arbDefCommand VDefCommand
                                 , arbGraphCommand
                                 ]
    where
      arbDataCommand       = liftA2 DataCommand arbDefines arbText
      arbDefCommand constr = liftA3 constr arbDefines arbStack arbReferences
      arbGraphCommand      = liftA2 GraphCommand arbText arbReferences

      arbDefines :: Gen Name
      arbDefines = fromTName <$> arbitrary

      arbText :: Gen String
      arbText = take 1000 . fromNonEmpty <$> arbitrary

      arbStack :: Gen [String]
      arbStack  =  take 20 . map (fromName . fromTName) . fromNonEmpty
               <$> arbitrary

      arbReferences :: Gen (S.Set Name)
      arbReferences = S.fromList . take 20 . map fromTName <$> arbitrary

  shrink (TCommand cmd) =
    map TCommand $ case cmd of
      DataCommand {} -> shrinkLens shrDefines cmdDefines cmd
                     ++ shrinkLens shrText    cmdText    cmd

      CDefCommand {} -> shrinkLens shrDefines    cmdDefines    cmd
                     ++ shrinkLens shrStack      cmdStack      cmd
                     ++ shrinkLens shrReferences cmdReferences cmd

      VDefCommand {} -> shrinkLens shrDefines    cmdDefines    cmd
                     ++ shrinkLens shrStack      cmdStack      cmd
                     ++ shrinkLens shrReferences cmdReferences cmd

      GraphCommand {} -> shrinkLens shrText       cmdText       cmd
                      ++ shrinkLens shrReferences cmdReferences cmd
    where
      shrinkLens :: (a -> [a]) -> (:->) f a -> f -> [f]
      shrinkLens shrinker l f = map (\a -> setL l a f) . shrinker $ getL l f

      shrDefines :: Name -> [Name]
      shrDefines = wrapShrink TName fromTName shrink

      shrText :: String -> [String]
      shrText = wrapShrink NonEmpty fromNonEmpty shrink

      shrStack :: [String] -> [[String]]
      shrStack = wrapShrink (map Name)  (map fromName)
               . wrapShrink (map TName) (map fromTName)
               . wrapShrink NonEmpty    fromNonEmpty
               $ shrink

      shrReferences :: S.Set Name -> [S.Set Name]
      shrReferences = wrapShrink S.toList    S.fromList
                    . wrapShrink (map TName) (map fromTName)
                    $ shrink

newtype TName = TName { fromTName :: Name }
  deriving (Eq, Ord, Read, Show)

instance Arbitrary TName where
  arbitrary =
    (TName . Name . map fromTNameChar . take 255 <$> listOf1 arbitrary)
      `suchThat` (\(TName name) -> nameIsValid name)

  shrink = filter (\(TName name) -> nameIsValid name)
         . ( wrapShrink fromTName       TName
           . wrapShrink fromName        Name
           . wrapShrink (map TNameChar) (map fromTNameChar)
           $ shrink )

newtype TNameChar = TNameChar { fromTNameChar :: Char }
  deriving (Eq, Ord, Read, Show)

instance Arbitrary TNameChar where
  arbitrary = TNameChar <$> elements chars
    where chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_"

  shrink (TNameChar 'a') = []
  shrink _               = [TNameChar 'a']

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

prop_formatCommand :: TCommand -> Bool
prop_formatCommand (TCommand c) =
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
        , intercalate "," <$> fLens cmdStack ]

-- Helpers.

wrapShrink :: (a -> b) -> (b -> a) -> (b -> [b]) -> a -> [a]
wrapShrink wrapper unwrapper shrinker = map unwrapper . shrinker . wrapper

fLens :: (:->) env a -> Field env a
fLens = asks . getL

fromNonEmpty :: NonEmptyList a -> [a]
fromNonEmpty (NonEmpty xs) = xs
