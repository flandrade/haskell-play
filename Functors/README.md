# Functors

A functor is a mapping between structures. We apply a function over or around some
structure that we don’t want to alter. Therefore, the function is applied to the
values inside the structure, preserving the structure.

## Definition in Haskell 

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

- `fmap` `(<$>)` is a specific sort of function application.

### Kind of `f`

Note that `f` here must have the kind `* -> *`. For example:

```haskell
data FooExample
  = Foo
  | Bar
deriving (Eq, Show)

instance Functor FooExample where
  fmap = undefined
```

Here `FooExample` is kind `*`, so we'll get an error. Therefore,
we should change the kind:

```haskell
data FooExample a
  = Foo
  | Bar a
deriving (Eq, Show)

instance Functor FooExample where
  fmap = undefined
```

Here `FooExample` is kind `* -> *`.

## Examples

List implements the typeclass Functor:

```haskell
fmap (\x -> x == 2) [1..3]
[False, True, False]
```

For lists, `fmap` is defined as:

```haskell
fmap :: (a -> b) -> [] a -> [] b
```

Maybe:

```haskell
fmap (\x -> x == 2) (Just 1)
Just False
``` 

For Maybe, `fmap` is defined as:

```haskell
fmap :: (a -> b) -> Maybe a -> Maybe b
```

## Laws 

The Functor typeclass should follow two laws: identity and composition.

### Identity 

If we `fmap` the identity function, we'll get the same result as passing the value
to identity.

```haskell
fmap id == id
```

```haskell
fmap id [1, 2, 3]
[1, 2, 3]
```

We can define this law in Haskell as follows:

```haskell
identity :: (Functor f, Eq (f a)) => f a -> Bool
identity f = fmap id f == f
```

### Composition

If we compose two functions (`f` and `g`), and fmap that over some structure,
we'll get the same result as fmapped them and then composed them.

```haskell
fmap (f . g) == fmap f . fmap g
```

```haskell
fmap ((*2) . (+2)) [1..3] === fmap (*2) . fmap (+ 2) $ [1..3]
true
```

We can define this law in Haskell as follows:

```haskell
composition :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
composition f g c = (fmap g (fmap f x)) == (fmap (g . f) x)
```

### Example of law-abiding instance

```haskell
data FooExample a 
  = Foo
  | OtherFoo
  | Bar a
deriving (Eq, Show)

instance Functor FooExample where 
  fmap _ Foo = Foo 
  fmap _ OtherFoo = OtherFoo
  fmap f (Bar a) = Bar (f a)
```

### Example of law-breaking instance

```haskell
instance Functor FooExample where 
  fmap _ Foo = OtherFoo
  fmap f OtherFoo = Foo
  fmap f (Bar a) = Bar (f a)
```

This breaks the identity law:

```haskell
fmap id Foo
OtherFoo
```

```haskell
fmap id Foo == id Foo
False
```

### Why is there no Functor instance for `Set`?

Sets allow to store unique, ordered elements. For this reason, `map` for `Set`
is defined as follows:

```haskell
map :: Ord b => (a->b) -> Set a -> Set b
```

According to the [documentation](https://hackage.haskell.org/package/containers-0.5.11.0/docs/Data-Set.html#v:map),
"it's worth noting that the size of the result may be smaller if, for some
`(x, y), x /= y && f x == f y`" 

Therefore, map cannot have a instance of Functor because it doesn't preserve
the structure.


## Bibliography:

- [Moronuki, Julie. Allen, Christopher. Haskell Programming from First Principles](http://haskellbook.com/)
