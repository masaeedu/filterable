# Data.Filterable

A categorical interpretation of what it means to be "filterable".

```
class Functor f => Filterable f
  where
  trivial :: f Void -> ()
  partition :: f (Either a b) -> (f a, f b)
```

A filterable functor is a symmetric *oplax* monoidal functor from `->`
under the `Either` monoidal structure to `->` under the `(,)` monoidal
structure.

This is nice, because it has sensible interactions with the concept of
`Alternative` functors, which are simply symmetric *lax* monoidal functors
between the same structures:

```
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
