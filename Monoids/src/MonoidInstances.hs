module MonoidInstances where

-- Sum 

newtype Sum n = Sum n deriving (Eq, Show)
  
instance Num n => Monoid (Sum n) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x + y)


-- List

data List n = Nil | Cons n (List n) deriving (Eq, Show)

instance Monoid (List n) where
  mempty = Nil
  mappend Nil n = n
  mappend n Nil = n
  mappend (Cons x xs) ys = Cons x $ xs `mappend` ys
