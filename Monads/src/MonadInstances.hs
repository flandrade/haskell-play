module MonadInstances where

import Data.Monoid


-- Identity

newtype Identity n = Identity n deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity n) = Identity (f n)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity n) = Identity (f n)

instance Monad Identity where
  return = pure
  (>>=) (Identity x) f = f x


-- Constant

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure x = Constant mempty
  (<*>) (Constant x) (Constant y) = Constant (x <> y)


-- List

data List n = Nil | Cons n (List n) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = append (fmap f xs) (fs <*> xs)
    where
      append Nil ys = ys
      append xs Nil = xs
      append (Cons x xs) ys = Cons x (append xs ys)
