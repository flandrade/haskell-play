module FunctorInstances where

-- Identity 

newtype Identity n = Identity n deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity n) = Identity (f n)


-- List

data List n = Nil | Cons n (List n) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)


-- Pair 

data Pair n = Pair n n deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)


-- Two

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)
