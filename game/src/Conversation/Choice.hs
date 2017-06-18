{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, TypeOperators, FlexibleContexts, FlexibleInstances #-}

module Conversation.Choice ( Choice
                           , CoChoice
                           , choice
                           , path
                           , displayChoice
                           ) where

import Prelude (zip, unzip, (!!)) -- Don't use the default
import Protolude  -- More minal, doesn't conflict
import Data.Text  -- Better than String, is based on arrays
import Control.Monad.Free
import Control.Monad.Trans.Free
import Data.Comp.Sum
import Data.Comp.Ops
import Control.Monad.Identity
import Data.DoList
import Control.Monad
import Control.Comonad
import Text.Read (readMaybe)
import Data.Maybe
import Data.List

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
    where (choices, paths) = unzip (fmap (\(Path c p) -> (c,p)) $ Data.DoList.toList options)

data CoChoice a = CoChoice ([Text] -> IO (Int, a)) deriving (Functor)

instance MonadIO m => PairingM CoChoice Choice m where
    pairM f (CoChoice p) (Choice choices paths) = do
        (i, a) <- liftIO (p choices)
        f a (paths!!i)

getValidOption :: Int    -- Min
               -> Int    -- Max
               -> IO Int
getValidOption  min max = do
    i <- getLine
    case readMaybe (unpack i) of
      Just n -> if n >= min && n <= max
                then pure n
                else getValidOption min max
      Nothing -> getValidOption min max

displayChoice :: Comonad w => w a -> CoChoice (w a)
displayChoice w = CoChoice $ \options -> do
    forM_ (Prelude.zip [1..] options) $ \(i, text) ->
      putStrLn (show i ++ " " ++ unpack text)
    i <- getValidOption 1 (Data.List.length options)
    pure (i - 1, w) -- Because lists are 0 indexed
