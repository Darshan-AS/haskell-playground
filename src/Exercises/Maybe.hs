module Exercises.Maybe where

import Prelude hiding (Just, Maybe, Nothing)

data Maybe a = Just a | Nothing
  deriving (Show, Eq)

instance Functor Maybe where
  fmap f (Just x) = Just $ f x
  fmap _ Nothing = Nothing

instance Applicative Maybe where
  pure = Just
  Just f <*> m = fmap f m
  Nothing <*> _ = Nothing

instance Monad Maybe where
  return = pure
  Just x >>= f = f x
  Nothing >>= _ = Nothing
