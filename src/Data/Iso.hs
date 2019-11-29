module Data.Iso where

import Prelude hiding (id, (.))

import Control.Category

data Iso p a b = Iso { fwd :: p a b, bwd :: p b a }

type (≅) = Iso (->)
infix 4 ≅

instance Category p => Category (Iso p)
  where
  id = Iso id id
  (Iso f f') . (Iso g g') = Iso (f <<< g) (f' >>> g')
