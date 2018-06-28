# Applicative Functors

A monoid is a monoidal functor that allows for function application

## Definition in Haskell

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

- `pure` embeds something into a an applicative structure.
- `<*>` `apply` lifts a function, which is embed into a structure, over an
applicative structure.

## Functors and applicatives

Monoids joins two values of the same type together into a joined structure. With
Funtor, we lift a function application over some value, which is embedded into a
structure, and doesn't alter the structure.

With Applicative, the function we're applying is also embedded into some
structure:

```haskell
  fmap :: (a -> b) -> f a -> f b
  (<*>) :: f (a -> b) -> f a -> f b
```

## Laws

An applicative functor is a function that takes two arguments and follows four
laws: identity,
composition, homomorphism and interchange.

### Identity

If we apply the identity function, we'll get the same result as passing the
value to identity.

```haskell
pure id <*> v = v
```

Example:

```haskell
pure id <*> [1, 2] === [1, 2]
pure id <*> Nothing === Nothing
```

We can define this law in Haskell as follows:

```haskell
leftIdentity :: (Eq m, Monoid m) => m -> Bool
leftIdentity a = (mempty <> a) == a

rightIdentity :: (Eq m, Monoid m) => m -> Bool
rightIdentity a = (a <> mempty) == a
```

### Composition

If we compose two functions (`v` and `w`), and apply to `<u>`, we'll get the
same result as applying the function first and then composing them.

```haskell
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

Example:

```haskell
pure (.) <*> Just (+5) <*> Just (*5) <*> Just 5 === Just (+5) <*> (Just (*5) <*> Just 5)
```

###  Homomorphism

It's a structure preserving map between two algebraic structures. That is,
applying a function,which is embedded in some structure, to a value, which is
also embedded, should be the same, as applying a function to a value without
affecting the structure.

```haskell
pure f <*> pure x= pure (f x)
```

Example:

```haskell
pure (+1) <*> pure 1 :: [Int]
```

### Interchange

```haskell
u <*> pure y = pure ($ y) <*> u
```

Examples:

```haskell
[(+1), (*2)] <*> pure 1 === pure ($ 1) <*> [(+1), (*2)]
````

## Bibliography:

- [Moronuki, Julie. Allen, Christopher. Haskell Programming from First Principles](http://haskellbook.com/)
