module Data.Bifunctor.Compose where

import Prelude

import Control.Biapplicative (class Biapplicative, bipure)
import Control.Biapply (class Biapply, biapply)

import Data.Bifunctor (class Bifunctor, bimap)

-- | The product of two `Bifunctor`s.
data Compose f g a b = Compose (f (g a b) (g a b))

derive instance eqCompose :: Eq (f (g a b) (g a b)) => Eq (Compose f g a b)

derive instance ordCompose :: Ord (f (g a b) (g a b)) => Ord (Compose f g a b)

instance showCompose :: Show (f (g a b) (g a b)) => Show (Compose f g a b) where
  show (Compose c) = "(Compose " <> show c <> ")"

instance bifunctorCompose :: (Bifunctor f, Bifunctor g) => Bifunctor (Compose f g) where
  bimap f g (Compose m) = Compose (bimap (bimap f g) (bimap f g) m)

instance biapplyCompose :: (Biapply f, Biapply g) => Biapply (Compose f g) where
  biapply (Compose f) (Compose a) = Compose (biapply (bimap biapply biapply f) a)

instance biapplicativeCompose :: (Biapplicative f, Biapplicative g) => Biapplicative (Compose f g) where
  bipure a b = Compose (bipure (bipure a b) (bipure a b))
