{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeApplications, TypeOperators, FlexibleContexts #-}

module Conversation.PlayerLine ( response
                               , PlayerLine(..)
                               ) where

import Prelude () -- Don't use the default
import Protolude ((.), ($), Show, join, Functor)  -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Comp.Sum
import Data.Comp.Ops
import Control.Monad.Identity

-- | A single player response
data PlayerLine a = PlayerLine Text a deriving (Show, Functor)

response :: (Functor f, MonadFree f m, PlayerLine :<: f)
         => Text
         -- ^ Player's response to NPC
         -> m ()
         -- ^ New conversation
response text = liftF . inj $ (PlayerLine text ())
