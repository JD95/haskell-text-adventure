{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, TypeOperators, FlexibleContexts, FlexibleInstances #-}

module Conversation.PlayerLine ( PlayerLine(..)
                               , CoPlayerLine
                               , player
                               , displayPlayerLine
                               ) where

import Prelude () -- Don't use the default
import Protolude ((.), ($), Show, liftIO, pure, (>>=), putStrLn, MonadIO, IO, Functor)  -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Comp.Sum
import Data.Comp.Ops
import Control.Monad.Identity
import Control.Comonad

import Pairing

-- | A single player response
data PlayerLine a = PlayerLine Text a
    deriving (Show, Functor)

player :: (Functor f, MonadFree f m, PlayerLine :<: f)
         => Text
         -- ^ Player's response to NPC
         -> m ()
         -- ^ New conversation
player text = liftF . inj $ (PlayerLine text ())

data CoPlayerLine a = CoPlayerLine (Text -> IO a)
    deriving (Functor)

-- | Pairs the printing function with the lines
instance MonadIO m => PairingM CoPlayerLine PlayerLine m where
    pairM f (CoPlayerLine p) (PlayerLine lines b) = liftIO (p lines) >>= (`f` b)

displayPlayerLine :: Comonad w => w a -> CoPlayerLine (w a)
displayPlayerLine w = CoPlayerLine $ \line -> putStrLn line >> pure w

