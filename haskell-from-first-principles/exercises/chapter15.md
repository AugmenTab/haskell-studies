# Chapter 15: Monoid, Semigroup

### Optional Monoid

Write the `Monoid` instance for our `Maybe` type, renamed to `Optional`.

```haskell
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only x) (Only y) = Only (x <> y)
  (<>) Nada     x        = x
  (<>) x        Nada     = x

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada
```
