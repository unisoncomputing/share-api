```ucm:hide
.> project.create-empty proj
proj/main> builtins.merge
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
proj/main> add
```

Push and pull it back.
```ucm
proj/main> push @transcripts/proj/main
proj/main> branch.create-empty pulled
proj/pulled> pull @transcripts/proj/main
proj/pulled> ls
```

```unison:hide
newValue = 99
```

Do a fast-forward push.
```ucm
proj/main> add
proj/main> push
```

Do a non-fast-forward push.

```ucm
proj/main> branch /diverge 
```

```unison:hide
diverge = 100
```

```ucm
proj/main> add
proj/main> push
```

```unison:hide
diverge = 200
```

```ucm:error
proj/diverge> add
proj/diverge> push @transcripts/proj/main
```

Pull to trigger local merge

```ucm:error
proj/diverge> pull @transcripts/proj/main
```
