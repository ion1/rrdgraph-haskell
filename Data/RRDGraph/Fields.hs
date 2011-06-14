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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Fields provides an API for serializing a data structure into a list of
-- fields by providing it with a list of accessors to the data.
--
-- Fields combines the semantics of the 'Reader' and 'Maybe' monads. The
-- accessors may result in 'Nothing' to represent a field to be skipped.
--
-- Examples:
--
-- >>> let dat = (Nothing, "foo")
--
-- Static fields can be added with 'pure'.
--
-- >>> catMaybes $ runFields [pure "static field"] dat
-- >["static field"]
--
-- When using OverloadedStrings, the above can be written as follows.
--
-- >>> catMaybes $ runFields ["static field"] dat
-- >["static field"]
--
-- As with 'Reader', one can use 'asks' to apply an accessor to the data.
--
-- >>> catMaybes $ runFields ["(", asks snd, ")"] dat
-- >["(", "foo", ")"]
--
-- Additionally, the function 'asksM' is provided for accessors that return a
-- 'Maybe' value.
--
-- >>> catMaybes $ runFields ["(", asksM fst, ")"] dat
-- >["(", ")"]
--
-- The type wraps the 'Monoid' instance of @'Maybe' a@, resulting in the
-- following behavior.
--
-- >>> catMaybes $ runFields [mempty `mappend` asksM fst] dat :: [String]
-- >[]
--
-- >>> catMaybes $ runFields ["baz" `mappend` asksM fst] dat
-- >["baz"]
--
-- >>> catMaybes $ runFields ["baz" `mappend` asks snd] dat
-- >["bazfoo"]
--
-- The usual 'Functor', 'Applicative' and 'Monad' instances are available.
--
-- >>> catMaybes $ runFields [map toUpper <$> asks snd] dat
-- >["FOO"]
--
-- >>> catMaybes $ runFields [asks snd <* asksM fst] dat
-- >[]
--
-- >>> catMaybes $ runFields [("is true" <$) . guard =<< asks fst] (False, True)
-- >[]
--
-- >>> catMaybes $ runFields [("is true" <$) . guard =<< asks snd] (False, True)
-- >["is true"]

module Data.RRDGraph.Fields
( Field (..)
, field
, runField
, runFields
, asks
, asksM
)
where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Functor.Identity
import Data.Monoid
import Data.String

newtype Field env a = Field (ReaderT env (MaybeT Identity) a)
  deriving (Functor, Applicative, Monad, MonadPlus, MonadReader env)

instance Monoid a => Monoid (Field env a) where
  mempty      = field $ const mempty
  mappend f g = field $ liftM2 mappend (runField f) (runField g)

instance IsString a => IsString (Field env a) where
  fromString = pure . fromString

-- | Wrap a function into Field.
field :: (env -> Maybe a) -> Field env a
field f = Field . ReaderT $ \env -> (MaybeT . Identity . f) env

-- | Run a single Field.
runField :: Field env a -> env -> Maybe a
runField (Field f) = runIdentity . runMaybeT . runReaderT f

-- | Run a list (or other functor) of Fields.
runFields :: Functor f => f (Field env a) -> env -> f (Maybe a)
runFields fs env = ($ env) . runField <$> fs

-- | Retrieve a function of the environment resulting in a Maybe value.
asksM :: (env -> Maybe a) -> Field env a
asksM = field
