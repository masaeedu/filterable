module Data.Applicative where

import Prelude hiding (Applicative, zip, all)
import Data.Bifunctor (first, second)
import Control.Category ((>>>))
import Data.MonoidalStructure (assocT, runitT, lunitT, (:*:))
import Data.Iso (fwd, bwd)

class Functor f => Applicative f
  where
  all       :: ()         -> f ()
  intersect :: f a :*: f b -> f (a :*: b)

testAssoc ::
  ( Applicative f
  , Eq (f (a :*: (b :*: c)))
  ) =>
  (f a :*: f b) :*: f c -> Bool
testAssoc x = f x == g x
  where
  f = first intersect >>> intersect >>> fmap (bwd assocT)
  g = (bwd assocT) >>> second intersect >>> intersect

testRUnit ::
  ( Applicative f
  , Eq (f a)
  ) =>
  f a :*: () -> Bool
testRUnit x = f x == g x
  where
  f = fwd runitT
  g = second all >>> intersect >>> fmap (fwd runitT)

testLUnit ::
  ( Applicative f
  , Eq (f a)
  ) =>
  () :*: f a -> Bool
testLUnit x = f x == g x
  where
  f = fwd lunitT
  g = first all >>> intersect >>> fmap (fwd lunitT)
