module Data.Alternative where

import Data.Void (Void)
import Control.Category ((>>>))
import Data.Bifunctor (first, second)
import Data.MonoidalStructure (assocT, assocE, runitT, runitE, lunitT, lunitE, (:+:), (:*:))
import Data.Iso (fwd, bwd)

class Functor f => Alternative f
  where
  empty :: ()          -> f Void
  union :: f a :*: f b -> f (a :+: b)

testAssoc ::
  ( Alternative f
  , Eq (f (a :+: (b :+: c)))
  ) =>
  (f a :*: f b) :*: f c -> Bool
testAssoc x = f x == g x
  where
  f = first union >>> union >>> fmap (bwd assocE)
  g = (bwd assocT) >>> second union >>> union

testRUnit ::
  ( Alternative f
  , Eq (f a)
  ) =>
  f a :*: () -> Bool
testRUnit x = f x == g x
  where
  f = fwd runitT
  g = second empty >>> union >>> fmap (fwd runitE)

testLUnit ::
  ( Alternative f
  , Eq (f a)
  ) =>
  () :*: f a -> Bool
testLUnit x = f x == g x
  where
  f = fwd lunitT
  g = first empty >>> union >>> fmap (fwd lunitE)
