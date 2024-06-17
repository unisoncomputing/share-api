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

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      type A
      type B
      a  : 'Nat
      b  : 'Nat
      xs : ['{g} Nat]
      ys : [Nat]

```
Push and pull it back.
```ucm
proj/main> push @transcripts/proj/main

  Uploaded 358 entities.

  I just created @transcripts/proj on http://localhost:5424

  View it here: @transcripts/proj/main on http://localhost:5424

proj/main> branch.create-empty pulled

  Done. I've created an empty branch proj/pulled.
  
  Tip: Use `merge /somebranch` to initialize this branch.

proj/pulled> pull @transcripts/proj/main

  ✅
  
  Successfully pulled into proj/pulled, which was empty.

proj/pulled> ls

  1. A        (type)
  2. A/       (2 terms)
  3. B        (type)
  4. B/       (2 terms)
  5. a        ('Nat)
  6. b        ('Nat)
  7. builtin/ (469 terms, 74 types)
  8. xs       (['{g} Nat])
  9. ys       ([Nat])

```
```unison
newValue = 99
```

Do a fast-forward push.
```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    newValue : Nat

proj/main> push

  Uploaded 3 entities.

  View it here: @transcripts/proj/main on http://localhost:5424

```
Do a non-fast-forward push.

```ucm
proj/main> branch /diverge 

  Done. I've created the diverge branch based off of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /diverge`.

```
```unison
diverge = 100
```

```ucm
proj/main> add

  ⍟ I've added these definitions:
  
    diverge : Nat

proj/main> push

  Uploaded 3 entities.

  View it here: @transcripts/proj/main on http://localhost:5424

```
```unison
diverge = 200
```

```ucm
proj/diverge> add

  ⍟ I've added these definitions:
  
    diverge : Nat

proj/diverge> push @transcripts/proj/main

  @transcripts/proj/main on http://localhost:5424 has some
  history that I don't know about.

```
Pull to trigger local merge

```ucm
proj/diverge> pull @transcripts/proj/main

  Merging...

  I couldn't automatically merge remote @transcripts/proj/main
  into proj/diverge. However, I've added the definitions that
  need attention to the top of scratch.u.

```
```unison:added-by-ucm scratch.u
-- proj/diverge
diverge : Nat
diverge = 200

-- @transcripts/proj/main
diverge : Nat
diverge = 100


```

