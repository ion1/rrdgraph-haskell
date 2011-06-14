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

module Data.RRDGraph.Command
( Command (..)
, Name (..)
, cmdDefines
, cmdReferences
, cmdStack
, cmdText
, formatCommand
)
where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.RRDGraph.Fields

import Data.Record.Label
import qualified Data.Set as S

data Command = DataCommand { _cmdDefines :: Name
                           , _cmdText    :: String
                           }
             | CDefCommand { _cmdDefines    :: Name
                           , _cmdStack      :: [String]
                           , _cmdReferences :: S.Set Name
                           }
             | VDefCommand { _cmdDefines    :: Name
                           , _cmdStack      :: [String]
                           , _cmdReferences :: S.Set Name
                           }
             | GraphCommand { _cmdText       :: String
                            , _cmdReferences :: S.Set Name
                            }
  deriving (Eq, Ord, Read, Show)

-- | An RRDtool variable name.
newtype Name = Name { fromName :: String }
  deriving (Eq, Ord, Read, Show)

mkLabels [''Command]

formatCommand :: Command -> String
formatCommand cmd =
  case cmd of
    DataCommand {} ->
      concat . catMaybes . flip runFields cmd $
        [ "DEF:", fromName <$> fLens cmdDefines, "=", fLens cmdText ]
    CDefCommand {}  -> formatDefCommand "CDEF" cmd
    VDefCommand {}  -> formatDefCommand "VDEF" cmd
    GraphCommand {} -> getL cmdText cmd

  where
    formatDefCommand prefix =
      concat . catMaybes . runFields
        [ prefix, ":", fromName <$> fLens cmdDefines, "="
        , intercalate "," <$> fLens cmdStack
        ]

fLens :: (:->) env a -> Field env a
fLens = asks . getL
