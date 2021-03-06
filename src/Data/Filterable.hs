{-# LANGUAGE AllowAmbiguousTypes, ImpredicativeTypes #-}

module Data.Filterable where

import GHC.Exts (Constraint)

import Data.Typeable
import Data.Void
import Data.Either
import Data.Bifunctor
import qualified Data.Map as M

import Control.Applicative hiding (Alternative(..))
import Control.Monad.Logic

import Test.QuickCheck

-- {{{ MONOIDAL STRUCTURES

assocE :: Either a (Either b c) -> Either (Either a b) c
assocE (Left a) = Left $ Left a
assocE (Right (Left b)) = Left $ Right b
assocE (Right (Right c)) = Right c

assocT :: (a, (b, c)) -> ((a, b), c)
assocT (a, (b, c)) = ((a, b), c)

swapE :: Either a b -> Either b a
swapE (Left x) = Right x
swapE (Right x) = Left x

swapT :: (a, b) -> (b, a)
swapT (a, b) = (b, a)

runitE :: a -> Either a Void
runitE = Left

runitT :: a -> (a, ())
runitT a = (a, ())

lunitE :: a -> Either Void a
lunitE = Right

lunitT :: a -> ((), a)
lunitT a = ((), a)

-- }}}

-- {{{ FILTERABLE CLASS

class Functor f => Filterable f
  where
  partition :: f (Either a b) -> (f a, f b)

trivial :: Filterable f => f Void -> ()
trivial = const ()

-- }}}

-- {{{ ALTERNATIVE CLASS

class Functor f => Alt f
  where
  alt :: f a -> f b -> f (Either a b)

class Alt f => Alternative f
  where
  empty :: f a

-- }}}

-- {{{ FILTERABLE CLASS LAWS

testAssoc :: (Eq (f a), Eq (f b), Eq (f c), Filterable f) => f (Either a (Either b c)) -> Bool
testAssoc = liftA2 (==) (first partition . partition . fmap assocE) (assocT . second partition . partition)

testRUnit :: (Filterable f, Eq (f a)) => f a -> Bool
testRUnit = liftA2 (==) runitT (bimap id trivial . partition . fmap runitE)

testLUnit :: (Filterable f, Eq (f a)) => f a -> Bool
testLUnit = liftA2 (==) lunitT (bimap trivial id . partition . fmap lunitE)

testSymmetry :: (Eq (f a), Eq (f b), Filterable f) => f (Either b a) -> Bool
testSymmetry = liftA2 (==) (partition . fmap swapE) (swapT . partition)

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

testQuestionable :: forall f. (TestF f, Filterable f, Applicative f, Alternative f) => Proxy f -> IO ()
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

instance Ord k => Filterable (M.Map k)
  where
  partition xs =
    ( M.foldMapWithKey (\k -> either (M.singleton k) (const M.empty)) $ xs
    , M.foldMapWithKey (\k -> either (const M.empty) (M.singleton k)) $ xs
    )

instance Filterable Logic
  where
  partition (LogicT x) = (LogicT $ \arr -> x $ either arr (const id), LogicT $ \brr -> x $ either (const id) brr)

-- }}}

-- {{{ ALTERNATIVE INSTANCES

instance Alt Maybe
  where
  Nothing `alt` Just a  = Just $ Right a
  Just a  `alt` _       = Just $ Left a
  Nothing `alt` Nothing = Nothing

instance Alternative Maybe
  where
  empty = Nothing

instance Alt []
  where
  xs `alt` ys = (Left <$> xs) ++ (Right <$> ys)

instance Alternative []
  where
  empty = []

instance Ord k => Alt (M.Map k)
  where
  xs `alt` ys = (Left <$> xs) `M.union` (Right <$> ys)

instance Ord k => Alternative (M.Map k)
  where
  empty = M.empty

-- }}}

observe' :: ((Logic a, Logic b), Logic c) -> (([a], [b]), [c])
observe' ((a, b), c) = ((observeAll a, observeAll b), observeAll c)
