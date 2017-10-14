{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Resource where

import GHC.TypeLits
import GHC.Prim
import Control.Monad.State.Lazy
import Control.Monad.IO.Class
import System.IO
import Control.Monad
import Data.Bifunctor

class RAII a where
  destruct :: a -> IO ()

instance RAII Int where
  destruct = const (pure ())

instance RAII String where
  destruct = const (pure ())

-- | Tags a resource with a symbol
data Resource (t :: Symbol) a where
  Resource :: (KnownSymbol t, RAII a) => Proxy# t -> a -> Resource t a

instance (KnownSymbol t, Show a) => Show (Resource t a) where
  show (Resource t a) = "(" ++ symbolVal' t ++ "," ++ show a ++ ")"

data Resources xs where
  None :: Resources '[]
  RCons :: (KnownSymbol t, RAII a) => a -> Resources as -> Resources (Resource t a : as)

instance Show (Resources '[]) where
  show None = "None"

instance (Show (Resources as), Show a) => Show (Resources (Resource t a : as)) where
  show (RCons a None) = show a
  show (RCons a rest) = show a ++ ", " ++ show rest

type family ResourcesConcatR (as :: [k]) (bs :: [k]) :: [k]
type instance ResourcesConcatR '[] x = x
type instance ResourcesConcatR (x : xs) ys = x : ResourcesConcatR xs ys

class ResourcesConcat as bs where
  resourcesConcat ::
       Resources as -> Resources bs -> Resources (ResourcesConcatR as bs)

instance ResourcesConcat '[] as where
  resourcesConcat None xs = xs

instance ResourcesConcat as bs where
  resourcesConcat (RCons a as) bs = RCons a (resourcesConcat as bs)

data ResourceM rs a = ResourceM (Resources rs) a deriving (Functor)

data Tag (t :: Symbol) a where
  Tag :: forall t a. KnownSymbol t => a -> Tag t a


(-:) ::
     forall t r rs a. (KnownSymbol t, RAII r)
  => IO (ResourceM rs a)
  -> Tag t (IO r)
  -> IO (ResourceM (Resource t r : rs) a)
prev -: (Tag mr) = prev >>= \(ResourceM rs a) -> fmap (\x -> ResourceM (RCons x rs) a) mr

con :: forall t r. (KnownSymbol t, RAII r) => IO r -> IO (ResourceM '[Resource t r] ())
con mr = fmap (\r -> ResourceM (RCons r None)  ()) mr


instance RAII Handle where
  destruct = hClose

test = con @"file" (openFile "test.txt" ReadMode)
    -: (Tag @"otherFile" (openFile "foo.txt" WriteMode))
    -: (Tag @"yetAnotherFile" (openFile "bar.txt" WriteMode))
