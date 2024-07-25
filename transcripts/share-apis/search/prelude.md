```unison
structural type List a = Nil | Cons a (List a)

function.const : a -> b -> a
function.const a b = a

structural ability Throw e where
  throw : e -> a

List.map : (a -> {g} b) -> List a -> {g} List b
List.map f = cases
  (Cons a rest) -> Cons (f a) (List.map f rest)
  Nil -> Nil
```


```ucm
scratch/main> add
scratch/main> push @transcripts/search/main
```
