{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeApplications, TypeOperators, FlexibleContexts #-}

module Conversation ( Conversation
                    , npc
                    , response
                    , choice
                    , path
                    ) where

import Prelude () -- Don't use the default
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Comp.Sum
import Data.Comp.Ops
import Control.Monad.Identity

import Conversation.NpcLine
import Conversation.Response
import Conversation.Choice

type Conversation_ = NpcLine :+: Response :+: Choice

-- | Models conversations between the player and some
--   Entity.
type Conversation a = FreeT Conversation_ Identity a


