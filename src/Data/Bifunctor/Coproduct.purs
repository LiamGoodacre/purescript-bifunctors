module Data.Bifunctor.Coproduct where

import Prelude

import Data.Bifunctor (class Bifunctor, bimap)

-- | The sum of two `Bifunctor`s.
data Coproduct f g a b = Inl (f a b) | Inr (g a b)

derive instance eqCoproduct :: (Eq (f a b), Eq (g a b)) => Eq (Coproduct f g a b)

derive instance ordCoproduct :: (Ord (f a b), Ord (g a b)) => Ord (Coproduct f g a b)

instance showCoproduct :: (Show (f a b), Show (g a b)) => Show (Coproduct f g a b) where
  show (Inl x) = "(Inl " <> show x <> ")"
  show (Inr y) = "(Inr " <> show y <> ")"

instance bifunctorCoproduct :: (Bifunctor f, Bifunctor g) => Bifunctor (Coproduct f g) where
  bimap f g (Inl x) = Inl (bimap f g x)
  bimap f g (Inr y) = Inr (bimap f g y)

