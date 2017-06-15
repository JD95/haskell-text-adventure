{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, TypeOperators, FlexibleContexts, FlexibleInstances #-}

module Conversation.Choice ( Choice
                           , CoChoice
                           , choice
                           , path
                           ) where

import Prelude (unzip, (!!)) -- Don't use the default
import Protolude ((.), ($), Int, Show, MonadIO, liftIO, IO, join, pure, Functor)  -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Comp.Sum
import Data.Comp.Ops
import Control.Monad.Identity
import Data.DoList

import Pairing
import Conversation.PlayerLine

-- | This will help ensure that chioces only have
--   paths and not any other part of conversation.
data Path a = Path Text a
    deriving (Show, Functor)

-- | Alternate paths for the player to take
data Choice a = Choice [Text] [a]
    deriving (Show, Functor)

path :: Text
     -> a
     -> DoList (Path a)
path choice p = item (Path choice p)

choice :: (Functor f, MonadFree f m, Choice :<: f)
       => DoList (Path (m ()))
       -- ^ Conversation paths as a result of this choice
       -> m ()
       -- ^ New converstaion
choice options = join . liftF . inj $ Choice choices paths
    where (choices, paths) = unzip (fmap (\(Path c p) -> (c,p)) $ toList options)

data CoChoice a = CoChoice ([Text] -> IO (Int, a)) deriving (Functor)

instance MonadIO m => PairingM CoChoice Choice m where
    pairM f (CoChoice p) (Choice choices paths) = do
        (i, a) <- liftIO (p choices)
        f a (paths!!i)
        
