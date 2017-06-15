{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeApplications, TypeOperators, FlexibleContexts #-}

module Conversation ( Conversation
                    , Display
                    , npc
                    , response
                    , choice
                    , path
                    ) where

import Prelude () -- Don't use the default
import Protolude (Int, IO)
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Control.Comonad.Trans.Cofree
import Data.Comp.Sum
import Data.Comp.Ops
import Control.Monad.Identity
import Control.Monad.State.Strict

import Conversation.NpcLine
import Conversation.PlayerLine
import Conversation.Choice

-- | Represents the player state
data GameState = GameState Int

-- | The underlying monad stack for the game
type Base = StateT GameState IO

-- | Create a sum type to represent conversation structure
type Conversation_ = NpcLine :+: PlayerLine :+: Choice

-- | Create a product type to represent displaying the conversation
type Display_ = CoNpcLine :*: CoPlayerLine :*: CoChoice

-- | Wrap our sum type into a Free Monad
type Conversation a = FreeT Conversation_ Base a

-- | Wrap our product type into a Cofree Monad
type Display a = CofreeT Display_ Base a

demo :: Conversation ()
demo = do
    npc ["Hello"]
    npc ["Good-bye"]
