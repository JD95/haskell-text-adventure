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
  (:.) :: (KnownSymbol t, RAII a) => a -> Resources as -> Resources (Resource t a : as)

infixr 6 :.

instance Show (Resources '[]) where
  show None = "None"

instance (Show (Resources as), Show a) => Show (Resources (Resource t a : as)) where
  show (a :. None) = show a
  show (a :. rest) = show a ++ ", " ++ show rest

type family ResourcesAppendR (as :: [k]) (bs :: [k]) :: [k]
type instance ResourcesAppendR '[] x = x
type instance ResourcesAppendR (x : xs) ys = x : ResourcesAppendR xs ys

class ResourcesAppend as bs where
  resourcesAppend ::
       Resources as -> Resources bs -> Resources (ResourcesAppendR as bs)

instance ResourcesAppend '[] as where
  resourcesAppend None xs = xs

instance ResourcesAppend as bs where
  resourcesAppend (a :. as) bs = a :. resourcesAppend as bs

data ResourceM rs a = ResourceM (Resources rs) a deriving (Functor)

(-:) ::
     forall t r rs a. (KnownSymbol t, RAII r)
  => ResourceM rs a
  -> IO r
  -> IO (ResourceM (Resource t r : rs) a)
(ResourceM rs a) -: mr = fmap (\r -> ResourceM (r :. rs) a) mr

resource :: forall t r. (KnownSymbol t, RAII r) => IO r -> IO (ResourceM '[Resource t r] ())
resource mr = fmap (\r -> ResourceM (r :. None)  ()) mr


instance RAII Handle where
  destruct = hClose

--test = foldM_ (-:) (resource @"file" (openFile "test.txt" ReadMode)) $
--  [resource @"otherFile" (openFile "othertest.txt" ReadMode)]


