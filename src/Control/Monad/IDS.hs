{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Control.Monad.IDS where

import           Control.Applicative
import           Control.Monad

import           GHC.Base            (build)
import           GHC.Exts
import           Numeric.Natural

newtype List a = List
  { runList :: forall b. (a -> b -> b) -> b -> b
  } deriving (Functor)

instance IsList (List a) where
  type Item (List a) = a
  fromList xs = List \f b -> foldr f b xs
  toList xs = build (runList xs)

instance Foldable List where
  foldr f b xs = runList xs f b

instance Applicative List where
  pure x = List \f -> f x
  xs <*> ys = List \f -> runList xs \x -> runList ys (f . x)

instance Monad List where
  xs >>= k = List \f -> runList xs \x -> runList (k x) f

instance Alternative List where
  empty = mempty
  (<|>) = (<>)

instance Semigroup (List a) where
  xs <> ys = List (\f -> runList xs f . runList ys f)

instance Monoid (List a) where
  mempty = List (\_ b -> b)

newtype Search a
  = Search
  { runSearch :: Natural -> List a
  } deriving (Functor, Semigroup, Monoid)

instance Applicative Search where
  pure x = Search \case
    0 -> pure x
    _ -> empty
  fs <*> xs =
    Search \n -> do
      i <- [0..n]
      let j = n - i
      f <- runSearch fs i
      x <- runSearch xs j
      pure (f x)

instance Alternative Search where
  empty = mempty
  (<|>) = (<>)

instance Monad Search where
  xs >>= f = Search \n -> do
    i <- [0..n]
    x <- runSearch xs i
    runSearch (f x) (n - i)

instance MonadPlus Search

toStream :: Search a -> [a]
toStream xs = toList ([0..] >>= runSearch xs)

-- searchFrom :: Natural -> Search Natural
-- searchFrom n = Search (pure . (+) n)

-- pyth :: Search (Natural,Natural,Natural)
-- pyth = do
--   x <- searchFrom 1
--   y <- searchFrom 1
--   z <- searchFrom 1
--   guard (x*x + y*y == z*z)
--   guard (x <= y)
--   guard (x > 10)
--   pure (x,y,z)


find :: Search a -> a
find xs = go 0
  where
    go !n = runList (runSearch xs n) const (go (n+1))
