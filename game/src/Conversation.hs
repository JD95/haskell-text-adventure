{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}

module Conversation ( Conversation
                    , Display
                    , npc
                    , player
                    , choice
                    , path
                    ) where

import Prelude () -- Don't use the default
import Protolude ((.), (<*>),($), Int, MonadIO, IO, pure, const, flip)
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Control.Comonad.Trans.Cofree
import Data.Comp.Sum
import Data.Comp.Ops
import Control.Monad.Identity
import Control.Comonad.Store

import Pairing
import Conversation.NpcLine
import Conversation.PlayerLine
import Conversation.Choice

-- | Represents the player state
newtype GameState =
  GameState Int

-- | The underlying monad stack for the game
type Base = StoreT GameState Identity

-- | Create a sum type to represent conversation structure
type Conversation_ = NpcLine :+: PlayerLine :+: Choice

-- | Create a product type to represent displaying the conversation
type Display_ = CoNpcLine :*: CoPlayerLine :*: CoChoice

-- | Wrap our sum type into a Free Monad
type Conversation a = FreeT Conversation_ IO a

-- | Wrap our product type into a Cofree Monad
type Display a = CofreeT Display_ Base a

demo :: Conversation ()
demo = do
  npc ["Hello there", "My name is bob!"]
  player "I'm daniel!"
  choice $ do
    path "I'm leaving now!" $ npc ["Oh, okay!"]
    path "Fuck off!" $ npc ["I see..."]
  npc ["Good-bye"]

-- | Displays a conversation
display :: Display (IO ())
display = coiterT next start
    where next = displayNpcLine *:* displayPlayerLine *:* displayChoice
          start = flip StoreT (GameState 0) . Identity $ const (pure ())

test :: IO ()
test = pairEffectM (flip const) display (fmap pure demo)

