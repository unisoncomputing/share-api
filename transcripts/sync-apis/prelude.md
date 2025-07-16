```ucm:hide
.prelude> builtins.mergeio
```

Create some types and values with deep-dependencies and cycles to ensure we have non-trivial components.

```unison
unique type A = SomeNat Nat | SomeB B

unique type B = SomeA A | SomeString Text

a = 'let
  !b + 1

b = 'let
  !a + 2

xs = [a, b]

ys = [!a, !b] :+ 3
```


```ucm
.prelude> update
```
