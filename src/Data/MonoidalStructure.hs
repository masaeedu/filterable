module Data.MonoidalStructure where

import Prelude hiding (id, (.))
import Data.Void
import Control.Category
import Data.Iso
import Data.Tuple (swap)

type (:+:) = Either
infixl 6 :+:

type (:*:) = (,)
infixl 7 :*:

assocE :: a :+: (b :+: c) ≅ (a :+: b) :+: c
assocE = Iso
  (either (Left <<< Left) (either (Left <<< Right) Right))
  (either (either Left (Right <<< Left)) (Right <<< Right))

assocT :: a :*: (b :*: c) ≅ (a :*: b) :*: c
assocT = Iso f g
  where
  f (a, (b, c)) = ((a, b), c)
  g ((a, b), c) = (a, (b, c))

swapE :: a :+: b ≅ b :+: a
swapE = Iso (either Right Left) (either Right Left)

swapT :: a :*: b ≅ b :*: a
swapT = Iso swap swap

runitE :: a :+: Void ≅ a
runitE = Iso (either id absurd) Left

lunitE :: Void :+: a ≅ a
lunitE = runitE <<< swapE

runitT :: a :*: () ≅ a
runitT = Iso fst (\x -> (x, ()))

lunitT :: () :*: a ≅ a
lunitT = runitT <<< swapT
