```ucm:hide
.source> builtins.merge
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


```ucm:hide
.source> add
```

Push and pull it back.
```ucm
.source> push.create transcripts.public.code
.source> pull transcripts.public.code .pulled
.pulled> ls
```

```unison:hide
newValue = 99
```

Do a fast-forward push.
```ucm
.source> add
.source> push transcripts.public.code
```

Do a non-fast-forward push.
```ucm
.> fork source source2
```

```unison:hide
source.ff1 = 100
source2.ff2 = 200
```

```ucm:error
.> add
.source> push transcripts.public.code
.source2> push transcripts.public.code
```

Pull and then try pushing again to resolve the issue.
```ucm
.source2> pull transcripts.public.code
.source2> push transcripts.public.code
```

Create and pull some unrelated history.
```unison:hide
unrelated1.foo = 100
unrelated2.foo = 200
```
```ucm
.> add
.> push.create transcripts.public.unrelated1 .unrelated1
.> pull transcripts.public.unrelated1 .unrelated2
```

It looks like we don't have any guard against pulling an unrelated branch to a local location.
