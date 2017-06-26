{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeOperators #-}

module Pairing
  ( Pairing(..)
  , PairingM(..)
  , pairEffect
  , pairEffectM
  , pairEffect'
  , (*:*)
  ) where

import           Control.Comonad (Comonad, extract)
import qualified Control.Monad.Free as F
import           Control.Monad.Trans.Free (FreeF(..), FreeT, runFreeT)
import qualified Control.Comonad.Cofree as C
import           Control.Comonad.Trans.Cofree
import           Data.Functor.Identity (Identity(..))
import           Data.Comp.Sum
import           Data.Comp.Ops
import           Control.Applicative

class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

instance Pairing f g => Pairing (C.Cofree f) (F.Free g) where
  pair p (a C.:< _) (F.Pure x) = p a x
  pair p (_ C.:< fs) (F.Free gs) = pair (pair p) fs gs

instance (Pairing f f', Pairing g g') => Pairing (f :+: g) (f' :*: g') where
  pair p (Inl x) (a :*: _) = pair p x a
  pair p (Inr x) (_ :*: b) = pair p x b

instance (Pairing f f', Pairing g g') => Pairing (f :*: g) (f' :+: g') where
  pair p (a :*: _) (Inl x) = pair p a x
  pair p (_ :*: b) (Inr x) = pair p b x


class (Functor f, Functor g, Monad m) => PairingM f g m where
  pairM :: (a -> b -> m r) -> f a -> g b -> m r

instance (Monad m) => PairingM ((,) (m a)) ((->) a) m where
  pairM p (ma, b) g = ma >>= \ a -> p b (g a)

instance (Monad m, PairingM f f' m, PairingM g g' m) => PairingM (f :+: g) (f' :*: g') m where
  pairM p (Inl x) (a :*: _) = pairM p x a
  pairM p (Inr x) (_ :*: b) = pairM p x b

instance (Monad m, PairingM f f' m, PairingM g g' m) => PairingM (f :*: g) (f' :+: g') m where
  pairM p (a :*: _) (Inl x) = pairM p a x
  pairM p (_ :*: b) (Inr x) = pairM p b x

instance (Monad m, Comonad w, PairingM f g m) => PairingM (CofreeT f w) (FreeT g m) m where
  pairM p c f = do
      let (a :< b) = extract . runCofreeT $ c
      f' <- runFreeT f
      case f' of
        Free x -> pairM (pairM p) b x
        Pure y -> p a y


pairEffect :: (Pairing f g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffect p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> return $ p (extract s) x
    Free gs -> pair (pairEffect p) (unwrap s) gs

pairEffect' :: (Pairing f g, Comonad w, Monad m)
           => (a -> b -> m r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffect' p s c = do
  mb <- runFreeT c
  case mb of
    Pure x -> p (extract s) x
    Free gs -> pair (pairEffect' p) (unwrap s) gs

pairEffectM :: (PairingM f g m, Comonad w, Monad m)
           => (a -> b -> m r) -> CofreeT f w (m a) -> FreeT g m b -> m r
pairEffectM p s c = do
  ma <- extract s
  mb <- runFreeT c
  case mb of
    Pure x -> p ma x
    Free gs -> pairM (pairEffectM p) (unwrap s) gs

(*:*) :: (Functor f, Functor g) => (a -> f a) -> (a -> g a) -> a -> (f :*: g) a
(*:*) = liftA2 (:*:)

infixr 8 *:*
