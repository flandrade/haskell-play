# Monoids and Semigroups

## Monoid

A monoid is an algebraic structure with an associative binary operation and has an identity element. 

### Definition in Haskell

```haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

- `mappend` `(<>)` defines how two values that inhabit the type can be joined together. 
- `mempty` defines the identity value for that mappend operation.
- `mconcat` `(++)` folds a list using the monoid.

### Examples

Here some common monoidal operations with lists: 

```haskell
mappend [1, 2] [8, 9]
[1, 2, 8, 9]
```

```haskell
mappend "Hi" " world"
"Hi world"
```

Note that `Integer` doesn't have a Monoid instance. It's not clear whether a numeric values 
should be added or multiplied as a mappend operation. For this reason, we have the Sum and 
Product newtypes to wrap these values and form a monoid.

Sum

```haskell
mappend (Sum 1) (Sum 3)
Sum {getSum = 4}
```

Product

```haskell
mappend (Product 1) (Product 3)
Product {getProduct = 3}
```

### Laws

A Monoid is a function that takes two arguments and follows two laws: associativity and identity.

#### Associativity 

We can associate the arguments of the operation differently and the result will be the same:

```haskell
mappend [1..3] [] = [1..3]
mappend [] [1..3] = [1..3]
```

In general:

```
\ a b c -> a + (b + c) == (a + b) + c
\ a b c -> (a + b) + c == a + (b + c)
```

We can define this law in Haskell as follows:

```haskell
associativity :: (Eq m, Monoid m) => m -> m -> m -> Bool 
associativity a b c = (a <> (b <> c)) == ((a <> b) <> c)
```

#### Identity

It's a value that turns the operation into the identity function.

```haskell
mappend x mempty = x 
mappend mempty x = x
```

In general:

```
\ a empty -> a empty == a
\ empty a -> empty a == a
```

We can define this law in Haskell as follows:

```haskell
leftIdentity :: (Eq m, Monoid m) => m -> Bool
leftIdentity a = (mempty <> a) == a

rightIdentity :: (Eq m, Monoid m) => m -> Bool 
rightIdentity a = (a <> mempty) == a
```

## Semigroup

A semigroup is an algebraic structure with an associative binary operation.

## Bibliography:

- [Moronuki, Julie. Allen, Christopher. Haskell Programming from First Principles](http://haskellbook.com/)
