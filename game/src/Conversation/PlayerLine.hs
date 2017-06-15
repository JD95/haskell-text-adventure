{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, TypeOperators, FlexibleContexts, FlexibleInstances #-}

module Conversation.PlayerLine ( PlayerLine(..)
                               , CoPlayerLine
                               , response
                               ) where

import Prelude () -- Don't use the default
import Protolude ((.), ($), Show, liftIO, MonadIO, IO, Functor)  -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Comp.Sum
import Data.Comp.Ops
import Control.Monad.Identity

import Pairing

-- | A single player response
data PlayerLine a = PlayerLine Text a
    deriving (Show, Functor)

response :: (Functor f, MonadFree f m, PlayerLine :<: f)
         => Text
         -- ^ Player's response to NPC
         -> m ()
         -- ^ New conversation
response text = liftF . inj $ (PlayerLine text ())

data CoPlayerLine a = CoPlayerLine (Text -> IO a)
    deriving (Functor)

-- | Pairs the printing function with the lines
instance MonadIO m => PairingM CoPlayerLine PlayerLine m where
    pairM f (CoPlayerLine p) (PlayerLine lines b) = liftIO (p lines) >>= \p' -> f p' b
