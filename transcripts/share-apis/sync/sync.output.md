``` ucm :hide
scratch/main> project.create-empty proj

proj/main> builtins.merge
```

Create some types and values with deep-dependencies and cycles to ensure we have non-trivial components.

``` unison
unique type A = SomeNat Nat | SomeB B

unique type B = SomeA A | SomeString Text

a = 'let
  !b + 1

b = 'let
  !a + 2

xs = [a, b]

ys = [!a, !b] :+ 3
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + type A
  + type B

  + a  : 'Nat
  + b  : 'Nat
  + xs : ['Nat]
  + ys : [Nat]

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
proj/main> update
```

Push and pull it back.

``` ucm
proj/main> push @transcripts/proj/main

  Uploaded 466 entities.

  I just created @transcripts/proj on http://localhost:5424

  View it here: @transcripts/proj/main on http://localhost:5424

proj/main> branch.create-empty pulled

  Done. I've created an empty branch proj/pulled.

  Tip: Use `merge /somebranch` to initialize this branch.

proj/pulled> pull @transcripts/proj/main

  ✅

  Successfully pulled into proj/pulled, which was empty.

proj/pulled> ls .

  1. A        (type)
  2. A.       (2 terms)
  3. B        (type)
  4. B.       (2 terms)
  5. a        ('Nat)
  6. b        ('Nat)
  7. builtin. (676 terms, 107 types)
  8. xs       (['Nat])
  9. ys       ([Nat])
```

``` unison :hide
newValue = 99
```

Do a fast-forward push.

``` ucm
proj/main> update

  Done.

proj/main> push

  Uploaded 3 entities.

  View it here: @transcripts/proj/main on http://localhost:5424
```

Do a non-fast-forward push.

``` ucm
proj/main> branch /diverge 

  Done. I've created the diverge branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /diverge`.
```

``` unison :hide
diverge = 100
```

``` ucm
proj/main> update

  Done.

proj/main> push @transcripts/proj/main

  Uploaded 3 entities.

  View it here: @transcripts/proj/main on http://localhost:5424
```

``` unison :hide
diverge = 200
```

``` ucm :error
proj/diverge> update

  Done.

proj/diverge> push @transcripts/proj/main

  @transcripts/proj/main on http://localhost:5424 has some
  history that I don't know about.
```

Pull to trigger local merge

``` ucm :error
proj/diverge> pull @transcripts/proj/main

  I couldn't automatically merge remote @transcripts/proj/main
  into proj/diverge. However, I've added the definitions that
  need attention to the top of scratch.u.

  When you're done, you can run

    update

  to merge your changes back into diverge and delete the
  temporary branch. Or, if you decide to cancel the merge
  instead, you can run

    cancel

  to delete the temporary branch and switch back to diverge.
```

``` unison :added-by-ucm scratch.u
-- proj/diverge
diverge : Nat
diverge = 200

-- @transcripts/proj/main
diverge : Nat
diverge = 100

```
