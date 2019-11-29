module Data.MonoidalStructure where

import Prelude hiding (id, (.))
import Data.Void
import Control.Category
import Data.Iso

type (:+:) = Either
infixl 6 :+:

type (:*:) = (,)
infixl 7 :*:

assocE :: a :+: (b :+: c) ≅ (a :+: b) :+: c
assocE = Iso _ _

assocT :: a :*: (b :*: c) ≅ (a :*: b) :*: c
assocT = Iso _ _

swapE :: a :+: b ≅ b :+: a
swapE = Iso _ _

swapT :: a :*: b ≅ b :*: a
swapT = Iso _ _

runitE :: a :+: Void ≅ a
runitE = Iso _ _

lunitE :: Void :+: a ≅ a
lunitE = runitE <<< swapE

runitT :: a :*: () ≅ a
runitT = Iso _ _

lunitT :: () :*: a ≅ a
lunitT = runitT <<< swapT
