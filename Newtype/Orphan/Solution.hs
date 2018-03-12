module Solution where

import OrphanA

newtype NewBook = NewBook Book

instance Eq NewBook where
  (==) (NewBook bookA) (NewBook bookB) = author bookA == author bookB
