```ucm
scratch/main> alias.type ##Nat data.Nat
scratch/main> alias.type ##Text data.Text
```

```unison
structural type data.List a = Nil | Cons a (List a)

-- Type with a shared prefix to List
structural type ListLike a = ListLike

function.const : a -> b -> a
function.const a b = a

structural ability Throw e where
  throw : e -> a

data.List.map : (a -> {g} b) -> List a -> {g} List b
data.List.map f = cases
  (Cons a rest) -> Cons (f a) (List.map f rest)
  Nil -> Nil

-- Stubs for types which have the same type mentions, but different return types.
Nat.toText : Nat -> Text
Nat.toText _n = "natText"

Nat.fromText : Text -> Nat
Nat.fromText _t = 0

-- A type which mentions a type that has a shared prefix with List
usesListLike : ListLike a -> ListLike b
usesListLike _ = ListLike
```

```ucm
scratch/main> add
scratch/main> push @transcripts/search/main
```
