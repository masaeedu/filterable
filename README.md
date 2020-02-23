# Data.Filterable

A categorical interpretation of what it means to be "filterable".

```haskell
class Functor f => Filterable f
  where
  trivial :: f Void -> ()
  partition :: f (Either a b) -> (f a, f b)
```

A filterable functor is a symmetric _oplax_ monoidal functor from `->`
under the `Either` monoidal structure to `->` under the `(,)` monoidal
structure.

This is nice, because it has sensible interactions with the concept of
`Alternative` functors, which are simply symmetric _lax_ monoidal functors
between the same structures:

```haskell
class Functor f => Alternative f
  where
  empty :: () -> f Void
  union :: (f a, f b) -> f (Either a b)
```

The laws simply follow from the definition of (co)-lax monoidal functors,
and somewhat surprisingly, manage to catch definitions of filtering that
"feel wrong". See the quickcheck stuff in the code (and try out some
"wrong" instance definitions to see if they're caught).

Some functors happen to only support one of `Filterable` and
`Alternative`, some support both, and some (fewer still), happen to
support both in a way such that the pairs of coherence maps form an
isomorphism. Such functors are strong monoidal functors between the
aforementioned monoidal structures.

## Interesting hypotheses

```haskell
instance (Monad f, Alternative f) => Filterable f
  where
  partition m = (m >>= either pure (const empty), m >>= either (const empty) pure)

-- Incidentally, this works out to what I wrote by hand and quickchecked for [] and Maybe
```

```haskell
class Functor f => Decide f
  where
  decide :: f (Either a b) -> Either (f a) (f b)

instance (Decide f, Alternative f) => Filterable f
  where
  partition m = either (, mempty) (mempty, ) $ decide m
```

Filterable seems to have something to do with the ability to write "conditional tautologies", as in the dichotomy between Hilbert style vs Gentzen style deduction.

TODO: Pursue this
