{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeApplications #-}

module Conversation ( Conversation
                    , npc
                    , response
                    , choice
                    , path) where

import Prelude () -- Don't use the default
import Protolude  -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free

-- | This is the type we will use for our Free Monad
data Conversation_ a
    = Npc [Text] a     -- ^ The lines and the next conversational item
    | Response Text a  -- ^ A single player response
    | Choice a         -- ^ Alternate paths for the player to take
      deriving (Show, Functor)

type Conversation a = Free Conversation_ a 

npc :: [Text]
    -- ^ The lines of dialogue 
    -> Conversation ()
    -- ^ New conversation
npc lines = liftF (Npc lines ())

response :: Text
         -- ^ Player's response to NPC
         -> Conversation ()
         -- ^ New conversation
response text = liftF (Response text ())

choice :: Conversation ()
       -- ^ Conversation paths as a result of this choice
       -> Conversation ()
       -- ^ New converstaion
choice options = join . liftF $ Choice options

path :: Text
     -- ^ The player's choice
     -> Conversation ()
     -- ^ Conversation as a result of choice
     -> Conversation ()
     -- ^ New conversation
path choice next = join . liftF $ (Response choice next)
