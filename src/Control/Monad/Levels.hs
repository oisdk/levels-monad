{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}

module Control.Monad.Levels where

import           Control.Applicative
import           Control.Monad
import           Data.Bag

newtype Levels a
  = Levels { runLevels :: [Bag a]
  } deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

unconsMon :: Monoid m => [m] -> (m, [m])
unconsMon (x:xs) = (x, xs)
unconsMon xs     = (mempty, xs)

infixr 5 :~
pattern (:~) :: Monoid m => m -> [m] -> [m]
pattern (:~) x xs <- (unconsMon -> ~(x, xs))
{-# COMPLETE (:~) #-}

instance Applicative Levels where
  pure = Levels . pure . pure
  _ <*> Levels [] = Levels []
  Levels xs <*> Levels (y0:ys) = Levels (foldr f [] xs)
    where
      f x zs = (x <*> y0) : foldr (g x) id ys zs
      g x y k (z :~ zs) = ((x <*> y) <> z) : k zs

instance Alternative Levels where
  empty = Levels []
  Levels xs <|> Levels ys = Levels (foldr f id xs ys)
    where
      f x xs' (y :~ ys') = (x <> y) : xs' ys'

instance MonadPlus Levels where

wrap :: Levels a -> Levels a
wrap (Levels xs@(_:_)) = Levels (mempty : xs)
wrap xs                = xs

instance Monad Levels where
  Levels xs >>= k = foldr f empty xs
    where
      f x ys = foldr ((<|>) . k) (wrap ys) x
