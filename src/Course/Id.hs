{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Id where

import           Course.Core

import           Control.Monad (ap, liftM)
import           Prelude       (Applicative (..), Functor (..), Monad (..))

newtype Id a = Id a deriving (Eq, Show)

runId :: Id a -> a
runId (Id a) = a

mapId :: (a -> b) -> Id a -> Id b
mapId f (Id a) = Id (f a)

bindId :: (a -> Id b) -> Id a -> Id b
bindId f (Id a) = f a

instance Functor Id where
    fmap = liftM

instance Applicative Id where
    (<*>) = ap
    pure = Id

instance Monad Id where
    (>>=) = flip bindId
    return = Id
