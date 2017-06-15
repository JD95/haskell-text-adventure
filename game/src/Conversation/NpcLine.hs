{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, TypeOperators, FlexibleInstances, FlexibleContexts #-}

module Conversation.NpcLine ( NpcLine
                            , CoNpcLine
                            , npc
                            , displayNpcLine
                            ) where

import Prelude () -- Don't use the default
import Protolude ( (.), ($), Show, Functor
                 , IO, MonadIO, liftIO, (>>=)
                 , putStrLn
                 )  -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Comp.Ops

import Pairing

-- | The lines and the next conversational item
data NpcLine a = Npc [Text] a deriving (Show, Functor)

npc :: ( Functor f      -- f must be a functor
       , MonadFree f m  -- Must be part of a Free monad
       , NpcLine :<: f) -- NpcLine has to be in f
    => [Text]
    -- ^ The lines of dialogue 
    -> m ()
    -- ^ New conversation
npc lines = liftF . inj $ Npc lines ()

-- | Wrapst he function for displaying an NPC line
data CoNpcLine a = CoNpc ([Text] -> IO a) deriving (Functor)


-- | Pairs the printing function with the lines
instance MonadIO m => PairingM CoNpcLine NpcLine m where
    pairM f (CoNpc p) (Npc lines b) = liftIO (p lines) >>= \p' -> f p' b

displayNpcLine :: [Text] -> IO ()
displayNpcLine = mapM_ putStrLn
