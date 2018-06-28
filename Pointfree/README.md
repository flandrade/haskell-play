# Pointfree Style

Programming style where we compose functions without specifying their argument.

Without pointfree:

```haskell
newId :: Int -> Int
newId n = id n
```

With pointfree:

```haskell
newId :: Int -> Int
newId = id
```

- Eta reductions simplifies a function definition. This leads to a point free style.
- Pointfree doesn't mean using more `(.)` operators. Therefore, "point" refer to a function's argument.

## Using `(.)` and `($)` operators

### `($)` Operator

This operator has the lowest possible precedence. It will evaluate everything to the right first and can be used to delay function application.

Type signature:

```haskell
($) :: (a -> b) -> a -> b
```

Definition:

```haskell
f $ a = fa
```

Infix precedence: 

```haskell
infixr 0 $
```

Example without parenthesis:

```haskell
(3^) (1 + 2)
27
```

Example with the `($)` operator:

```haskell
(2^) $ 2 + 2
27
```

### `(.)` Operator

This operator has the greatest possible precedence. It indicates function composition. For example, `(f. g. h) x` is easier to read than `f (g (h x))`.

Type signature:

```haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
```

Definition:

```haskell
(f . g) x = f (g x)
```

Infix precedence: 

```haskell
infixr 9 .
```

Example without the `(.)` operator:

```haskell
negateAndSum :: (Num c, Foldable t) => t c -> c
negateAndSum list = negate (sum list)
```

Example with the `(.)` operator:

```haskell
negateAndSum :: (Num c, Foldable t) => t c -> c
negateAndSum = negate . sum
```

### More examples

```haskell
fx ::  Eq a => a -> [a] -> Int
fx x list = length $ filter (== x) list
```

```haskell
fx2::  Eq a => a -> [a] -> Int
fx2 x list = length (filter (== x) list)
```

```haskell
fx3 ::  Eq a => a -> [a] -> Int
fx3 x = length . filter (== x)
```

## Bibliography:

- [Pointfree](https://wiki.haskell.org/Pointfree)
- [Moronuki, Julie. Allen, Christopher. Haskell Programming from First Principles](http://haskellbook.com/)
