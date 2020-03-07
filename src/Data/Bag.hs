{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Bag where

import           Control.Applicative
import           Control.Monad

import           Data.Data            (Data, Typeable)
import           GHC.Generics         (Generic)

import           Data.Function        (on)
import           Data.List            (sort)

import           Data.Functor.Classes

newtype Bag a = Bag
  { runBag :: [a] }
  deriving stock (Show, Read, Functor, Foldable, Traversable, Generic, Data, Typeable)
  deriving newtype (Applicative, Monad, Alternative, MonadPlus, Semigroup, Monoid)

instance Ord a => Eq (Bag a) where
  (==) = (==) `on` (sort . runBag)

instance Ord a => Ord (Bag a) where
  compare = compare `on` (sort . runBag)

instance Eq1 Bag where
  liftEq eq (Bag xs) (Bag ys') = foldr f null xs ys'
    where
      f _ _ [] = False
      f x k (y:ys)
        | eq x y = k ys
        | otherwise = f x (k . (y:)) ys
