{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, TypeOperators,
  FlexibleInstances, FlexibleContexts, GADTs #-}

module Conversation.NpcLine
  ( NpcLine
  , CoNpcLine
  , npc
  , displayNpcLine
  ) where

import Prelude () -- Don't use the default
import Protolude
       (Functor, IO, MonadIO, Show, ($), (.), (>>=), liftIO, pure,
        putStrLn)
                    -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad
import Control.Monad.Free
import Control.Comonad
import Data.Comp.Ops

import Pairing

-- | The lines and the next conversational item
data NpcLine a =
  Npc [Text] a
  deriving (Show, Functor)

npc ::
     ( Functor f -- f must be a functor
     , MonadFree f m -- Must be part of a Free monad
     , NpcLine :<: f -- NpcLine has to be in f
     )
  => [Text]
    -- ^ The lines of dialogue
  -> m ()
    -- ^ New conversation
npc lines = liftF . inj $ Npc lines ()

-- | Wrapst he function for displaying an NPC line
newtype CoNpcLine a =
  CoNpc ([Text] -> IO a)
  deriving (Functor)

-- | Pairs the printing function with the lines
instance MonadIO m => PairingM CoNpcLine NpcLine m where
  pairM f (CoNpc p) (Npc lines b) = liftIO (p lines) >>= (`f` b)

displayNpcLine :: Comonad w => w a -> CoNpcLine (w a)
displayNpcLine w = CoNpc $ \lines -> mapM_ putStrLn lines >> pure w
