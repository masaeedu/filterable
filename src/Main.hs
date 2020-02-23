module Main where

import Data.Proxy
import Control.Applicative
import Data.Bifunctor
import Data.Map (Map)

import Control.Monad.Logic

import Data.Filterable

main :: IO ()
main = do
  testFilterable $ Proxy @Maybe
  testQuestionable $ Proxy @Maybe

  testFilterable $ Proxy @[]
  testQuestionable $ Proxy @[]

  testFilterable $ Proxy @(Map String)

  -- testFilterable $ Proxy @Logic
  let
    l :: Logic (Either Int (Either Int Int))
    l = (pure $ Left 1) <|> (pure $ Right $ Right 3)
  print $ observe' $ first partition . partition . fmap assocE $ l
  print $ observe' $ assocT . second partition . partition     $ l
