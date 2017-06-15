{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeApplications, TypeOperators, FlexibleContexts #-}

module Conversation.Choice ( Choice
                           , choice
                           , path) where

import Prelude () -- Don't use the default
import Protolude ((.), ($), Show, join, Functor)  -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Comp.Sum
import Data.Comp.Ops
import Control.Monad.Identity

import Conversation.PlayerLine

-- | Alternate paths for the player to take
data Choice a = Choice a deriving (Show, Functor)

choice :: (Functor f, MonadFree f m, Choice :<: f)
       => m ()
       -- ^ Conversation paths as a result of this choice
       -> m ()
       -- ^ New converstaion
choice options = join . liftF . inj $ Choice options

path :: (Functor f, MonadFree f m, PlayerLine :<: f)
     => Text
     -- ^ The player's choice
     -> m ()
     -- ^ Conversation as a result of choice
     -> m ()
     -- ^ New conversation
path choice next = join . liftF . inj $ (PlayerLine choice next)
