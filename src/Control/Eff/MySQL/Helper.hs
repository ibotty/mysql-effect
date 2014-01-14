{-# LANGUAGE FlexibleContexts #-}
module Control.Eff.MySQL.Helper
  ( askLift0
  , askLift
  , askLift2
  , askLift3
  ) where

import Control.Eff
import Control.Eff.Lift
import Control.Eff.Reader.Strict
import Data.Typeable (Typeable, Typeable1)

askLift0
  :: (Typeable a, Typeable1 m, SetMember Lift (Lift m) r, Member (Reader a) r)
  => (a -> m b) -> Eff r b
askLift0 f = ask >>= lift . f

askLift
  :: (Typeable a, Typeable1 m, SetMember Lift (Lift m) r, Member (Reader a) r)
  => (a -> t -> m b) -> t -> Eff r b
askLift f a = ask >>= \x -> lift (f x a)

askLift2
  :: (Typeable a, Typeable1 m, SetMember Lift (Lift m) r, Member (Reader a) r)
  => (a -> t -> t1 -> m b) -> t -> t1 -> Eff r b
askLift2 f a b = ask >>= \x -> lift (f x a b)

askLift3
  :: (Typeable a, Typeable1 m, SetMember Lift (Lift m) r, Member (Reader a) r)
  => (a -> t -> t1 -> t2 -> m b) -> t -> t1 -> t2 -> Eff r b
askLift3 f a b c = ask >>= \x -> lift (f x a b c)
