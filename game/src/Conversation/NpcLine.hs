{-# LANGUAGE DeriveFunctor, OverloadedStrings, TypeApplications, TypeOperators, FlexibleContexts #-}

module Conversation.NpcLine ( NpcLine
                            , npc
                            ) where

import Prelude () -- Don't use the default
import Protolude ((.), ($), Show, Functor, IO)  -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Comp.Ops

-- | The lines and the next conversational item
data NpcLine a = Npc [Text] a deriving (Show, Functor)

npc :: (Functor f, MonadFree f m, NpcLine :<: f)
    => [Text]
    -- ^ The lines of dialogue 
    -> m ()
    -- ^ New conversation
npc lines = liftF . inj $ Npc lines ()

data CoNpcLine a = CoNpc ([Text] -> IO ()) a deriving (Functor)


