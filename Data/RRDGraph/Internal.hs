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

-- | Miscellaneous internal utility functions.
module Data.RRDGraph.Internal
( fLens
, fLensM
, shrinkLens
, wrapShrink
, numericField
, numUniques
, (.:)
)
where

import Data.RRDGraph.Fields

import Data.Record.Label
import qualified Data.Set as S

fLens :: (:->) env a -> Field env a
fLens = asks . getL

fLensM :: (:->) env (Maybe a) -> Field env a
fLensM = asksM . getL

shrinkLens :: (a -> [a]) -> (:->) f a -> f -> [f]
shrinkLens shrinker l f = map (\a -> setL l a f) . shrinker $ getL l f

wrapShrink :: (a -> b) -> (b -> a) -> (b -> [b]) -> a -> [a]
wrapShrink wrapper unwrapper shrinker = map unwrapper . shrinker . wrapper

numericField :: Real a => a -> String
numericField = (show :: Double -> String) . realToFrac

numUniques :: Ord a => [a] -> Int
numUniques = S.size . S.fromList

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g a b = f (g a b)
