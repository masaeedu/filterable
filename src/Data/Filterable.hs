{-# LANGUAGE AllowAmbiguousTypes, ImpredicativeTypes #-}

module Data.Filterable where

import GHC.Exts

import Data.Typeable
import Data.Void
import Data.Either
import Data.Bifunctor
import Control.Applicative
import Control.Monad.Logic
import Test.QuickCheck
import Data.MonoidalStructure (assocT, assocE, runitT, runitE, lunitT, lunitE, swapT, swapE)
import Data.Iso (fwd, bwd)

class Functor f => Filterable f
  where
  partition :: f (Either a b) -> (f a, f b)

trivial :: Filterable f => f Void -> ()
trivial = const ()

testAssoc :: (Eq (f a), Eq (f b), Eq (f c), Filterable f) => f (Either a (Either b c)) -> Bool
testAssoc x = f x == g x
  where
  f = first partition . partition . fmap (fwd assocE)
  g = (fwd assocT) . second partition . partition

testRUnit :: (Filterable f, Eq (f a)) => f a -> Bool
testRUnit x = f x == g x
  where
  f = bwd runitT
  g = second trivial . partition . fmap (bwd runitE)

testLUnit :: (Filterable f, Eq (f a)) => f a -> Bool
testLUnit x = f x == g x
  where
  f = bwd lunitT
  g = bimap trivial id . partition . fmap (bwd lunitE)

testSymmetry :: (Eq (f a), Eq (f b), Filterable f) => f (Either b a) -> Bool
testSymmetry x = f x == g x
  where
  f = partition . fmap (fwd swapE)
  g = (fwd swapT) . partition

type TestF f =
  ( Typeable f
  , (forall a. Arbitrary a => Arbitrary (f a) :: Constraint)
  , (forall a. Show      a => Show      (f a) :: Constraint)
  , (forall a. Eq        a => Eq        (f a) :: Constraint)
  )

testFilterable :: forall f. (TestF f, Filterable f) => Proxy f -> IO ()
testFilterable _ = do
  putStrLn $ "testing whether " ++ (show $ typeRep $ Proxy @f) ++ " is a valid Filterable"

  putStrLn "associativity:"
  quickCheck $ testAssoc @f @Int @Int @Int

  putStrLn "left unit:"
  quickCheck $ testRUnit @f @Int

  putStrLn "right unit:"
  quickCheck $ testLUnit @f @Int

  putStrLn "symmetry:"
  quickCheck $ testSymmetry @f @Int @Int

-- }}}

-- {{{ FILTERABLE DERIVED OPERATIONS

filter :: Filterable f => (a -> Bool) -> f a -> f a
filter p = fst . partition . fmap (\x -> if p x then Left x else Right ())

fmapMaybe :: Filterable f => (a -> Maybe b) -> f a -> f b
fmapMaybe f = snd . partition . fmap (maybe (Left ()) Right) . fmap f

empty' :: (Filterable f, Applicative f) => f a
empty' = snd $ partition $ pure $ Left ()

-- }}}

-- {{{ QUESTIONABLE LAWS

testEmpty :: forall f a b. (Filterable f, Alternative f, Eq (f a), Eq (f b)) => Bool
testEmpty = partition (empty @f @(Either a b)) == (empty, empty)

testEmpty' :: forall f a b. (Filterable f, Applicative f, Eq (f a), Eq (f b)) => Bool
testEmpty' = partition (empty' @f @(Either a b)) == (empty', empty')

testQuestionable :: forall f. (TestF f, Filterable f, Alternative f) => Proxy f -> IO ()
testQuestionable _ = do
  quickCheck $ testEmpty @f @Int @Int
  quickCheck $ testEmpty' @f @Int @Int

-- }}}

-- {{{ FILTERABLE INSTANCES

instance Filterable Maybe
  where
  partition Nothing          = (Nothing, Nothing)
  partition (Just (Left x))  = (Just x, Nothing)
  partition (Just (Right x)) = (Nothing, Just x)

instance Filterable []
  where
  partition xs = (lefts xs, rights xs)

instance Filterable Logic
  where
  partition (LogicT x) = (LogicT $ \arr -> x $ either arr (const id), LogicT $ \brr -> x $ either (const id) brr)

-- }}}

observe' :: ((Logic a, Logic b), Logic c) -> (([a], [b]), [c])
observe' ((a, b), c) = ((observeAll a, observeAll b), observeAll c)

main :: IO ()
main = do
  testFilterable $ Proxy @Maybe
  testQuestionable $ Proxy @Maybe

  testFilterable $ Proxy @[]
  testQuestionable $ Proxy @[]

  -- testFilterable $ Proxy @Logic
  let
    l :: Logic (Either Int (Either Int Int))
    l = (pure $ Left 1) <|> (pure $ Right $ Right 3)
  print $ observe' $ first partition . partition . fmap (fwd assocE) $ l
  print $ observe' $ (fwd assocT) . second partition . partition     $ l
