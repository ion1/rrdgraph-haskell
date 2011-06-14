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

{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Data.RRDGraph.State
( GraphState (..)
, runGraph
, runGraphRaw
, runGraphState
, evalGraphState
, execGraphState
, newName
, addCommand
)
where

import Data.RRDGraph.Command

import Control.Applicative
import Control.Monad.State
import Data.Default
import qualified Data.DList as DL
import Data.Record.Label

-- | The state monad used to build a list of RRDtool commands.
newtype GraphState a = GraphState { fromGraphState :: State GraphStateData a }
  deriving ( Functor, Applicative, Monad, MonadFix
           , MonadState GraphStateData )

-- | The actual state data for 'GraphState'.
data GraphStateData = GraphStateData { _gsdCounter  :: Integer
                                     , _gsdCommands :: DL.DList Command
                                     }

instance Default GraphStateData where
  def = GraphStateData def defDList where defDList = DL.empty

mkLabels [''GraphStateData]

-- | Run a 'GraphState' computation, returning the resulting list of RRDtool
-- commands as strings.
runGraph :: GraphState a -> [String]
runGraph = map formatCommand . runGraphRaw

-- | Run a 'GraphState' computation, returning the resulting list of 'Command'
-- structures.
runGraphRaw :: GraphState a -> [Command]
runGraphRaw = DL.toList . getL gsdCommands . execGraphState

-- | Run a 'GraphState' computation, returning the result value and the
-- resulting state.
runGraphState :: GraphState a -> (a, GraphStateData)
runGraphState = flip runState def . fromGraphState

-- | Run a 'GraphState' computation, returning the result value.
evalGraphState :: GraphState a -> a
evalGraphState = flip evalState def . fromGraphState

-- | Run a 'GraphState' computation, returning the resulting state.
execGraphState :: GraphState a -> GraphStateData
execGraphState = flip execState def . fromGraphState

-- | Generate a unique RRDtool variable name.
newName :: GraphState Name
newName = Name . ("v" ++) . show <$> getM gsdCounter <* modM gsdCounter (+1)

-- | Add a 'Command' to the resulting list of commands.
addCommand :: Command -> GraphState ()
addCommand = modM gsdCommands . flip DL.snoc
