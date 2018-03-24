# Monads

A monad is an applicative functor. It is implemented as a typeclass with two
methods, `return` and `(>>=)`.

## Definition

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) - >m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

- `return` injects a value of type `a` into a monadic context.
- `(>>=)` bind operator lifts a monadic function over an structure and joins
the structure.
- `>>` is a sequencing function.

### Example

```haskell
data Maybe a = Nothing | Just a

instance Monad Maybe where
  (Just x) >>= k = k x
  Nothing  >>= k = Nothing

  return = Just
```

In particular,

```haskell
(>>=) :: Monad m => m a -> (a -> m b) -> mb
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

return :: Monad m => a -> m a
return :: a -> Maybe a
```

## Monadic function

A monadic function generates more structure after having been lifted over
manidic structure. The bind operator alter the structure after it.

```haskell
(>>=) :: m a -> (a -> m b) -> m b
```

In general, Monads get a value with type `m a` and returns a function with type
`(a -> m b)`, and `>>=` results in a final value of type `m b`.

While applicatives and functors have to leave the structure untouched, monads
can alter the structure. We can inject more structur with function application
and flatten two layers of structure into one.

`>>=` puts together the join function with the mapping function.

Here we lift `(a -> b)` over `f a`:

```haskell
fmap :: (a -> b) -> f a -> f b
````

Let's check the join, where we flatten the two layers of structure into one:

```haskell
join :: Monad m => m (m a) -> m a
```

## Sequence notation

Monadic syntax in Haskell is written in a sugared form known as do notation.

```haskell
(*>) :: Applicative f => f a -> f b -> f b

(>>) :: Monad m => m a -> m b -> m b
```

`*>` and `>>` sequence functions.

```haskell
sequence :: IO ()
sequence =
  putStrLn "Hello" >> putStrLn " World!"
```

is equal to

```haskell
sequence :: IO ()
sequence =
  putStrLn "Hello" *> putStrLn " World!"
```

and equal to

```haskell
sequence :: IO ()
sequence = do
  putStrLn "Hello"
  putStrLn " World!"
```

We can do the same with binding. The result of one actions is the input value
to the next action.

```haskell
sequence :: IO ()
sequence = do
  something <- getLine
  putStrLn something
```

is equal to

```haskell
sequence :: IO ()
sequence = getLine >>= putStrLn
```

With monads, we can use functions that depend on a previous value and returns
more monadic structure in its return type. Do syntax makes this cleaner.

## Laws

An applicative functor is a function that takes two arguments and follows three
laws: two identity lows and associativity.

### Identity

Return is neutra, so there are two identify laws to ensure this:

Right identity

```haskell
m >>= return = m
```

Left identity

```haskell
x >>= f = fx
```

where `return :: a -> m a`

### Associativity

```haskell
(m>>=f) >>= g = m >>= (\x -> fx >>= g)
```

## Bibliography:

- [Moronuki, Julie. Allen, Christopher. Haskell Programming from First Principles](http://haskellbook.com/)
- [Diehl, Stephen. What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/#monadic-methods)
