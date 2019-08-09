{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a)) deriving (Show, Eq)

instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  (<$>) ::
    (a -> b)
    -> Compose f g a
    -> Compose f g b
  f <$> (Compose a) =
    Compose $ (f <$>) <$> a

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
  pure ::
    a ->
    Compose f g a
  pure =
    Compose . pure . pure
  (<*>) ::
    Compose f g (a -> b)
    -> Compose f g a
    -> Compose f g b
  (Compose f) <*> (Compose a) =
    Compose $ lift2 (<*>) f a

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "not implementable"
