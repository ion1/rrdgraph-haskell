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
, addCommandDef
)
where

import Data.RRDGraph.Command

import Control.Applicative
import Control.Monad.State
import Data.Default
import qualified Data.DList as DL
import qualified Data.Map as M
import Data.Record.Label

-- | The state monad used to build a list of RRDtool commands.
newtype GraphState a = GraphState { fromGraphState :: State GraphStateData a }
  deriving ( Functor, Applicative, Monad, MonadFix
           , MonadState GraphStateData )

-- | The actual state data for 'GraphState'.
data GraphStateData = GraphStateData { _gsdCounter  :: Integer
                                     , _gsdDuplMap  :: M.Map Command Name
                                     , _gsdCommands :: DL.DList Command
                                     }

instance Default GraphStateData where
  def = GraphStateData def def defDList where defDList = DL.empty

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

-- | Given a 'Command' that defines an RRDtool variable, look up whether it has
-- already been added. If it’s a duplicate, change nothing and result in the
-- previously allocated name. If it’s a new command, allocate a new name, add
-- the command with the name and result in the name.
addCommandDef :: Command -> GraphState Name
addCommandDef cmd_ =
  case cmd_ of
    DataCommand {} -> addCommandDef' cmd_
    CDefCommand {} -> addCommandDef' cmd_
    VDefCommand {} -> addCommandDef' cmd_
    GraphCommand {} ->
      error "addCommandDef only applies to commands with cmdDefines"

  where
    addCommandDef' :: Command -> GraphState Name
    addCommandDef' cmd = do
      let cmd' = setL cmdDefines (Name "") cmd
      maybeName <- lookupDuplMap cmd'
      case maybeName of
        Just name -> return name
        Nothing   -> do
          name <- newName
          insertDuplMap cmd' name
          addCommand $ setL cmdDefines name cmd'
          return name

    lookupDuplMap :: Command -> GraphState (Maybe Name)
    lookupDuplMap name = M.lookup name <$> getM gsdDuplMap

    insertDuplMap :: Command -> Name -> GraphState ()
    insertDuplMap = modM gsdDuplMap .: M.insert

-- Helpers.

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g a b = f (g a b)
