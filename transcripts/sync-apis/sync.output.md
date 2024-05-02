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
.source> push.create transcripts.public.code

  Uploaded 322 entities.

  View it on Unison Share: https://share.unison-lang.org/@transcripts/p/code/latest/namespaces/public/code

.source> pull transcripts.public.code .pulled

  Downloaded 0 entities.

  ✅
  
  Successfully pulled into .pulled, which was empty.

.pulled> ls

  1. A        (type)
  2. A/       (2 terms)
  3. B        (type)
  4. B/       (2 terms)
  5. a        ('Nat)
  6. b        ('Nat)
  7. builtin/ (455 terms, 71 types)
  8. xs       (['{g} Nat])
  9. ys       ([Nat])

```
```unison
newValue = 99
```

Do a fast-forward push.
```ucm
.source> add

  ⍟ I've added these definitions:
  
    newValue : Nat

.source> push transcripts.public.code

  Uploaded 3 entities.

  View it on Unison Share: https://share.unison-lang.org/@transcripts/p/code/latest/namespaces/public/code

```
Do a non-fast-forward push.
```ucm
.> fork source source2

  Done.

```
```unison
source.ff1 = 100
source2.ff2 = 200
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    source.ff1  : Nat
    source2.ff2 : Nat

.source> push transcripts.public.code

  Uploaded 3 entities.

  View it on Unison Share: https://share.unison-lang.org/@transcripts/p/code/latest/namespaces/public/code

.source2> push transcripts.public.code

  ❗️
  
  There are some changes at transcripts.public.code that aren't
  in the history you pushed.
  
  If you're sure you got the right paths, try `pull` to merge
  these changes locally, then `push` again.

```
Pull and then try pushing again to resolve the issue.
```ucm
.source2> pull transcripts.public.code

  Downloaded 0 entities.

  Merging...

  Applying changes from patch...

.source2> push transcripts.public.code

  Uploaded 5 entities.

  View it on Unison Share: https://share.unison-lang.org/@transcripts/p/code/latest/namespaces/public/code

```
Create and pull some unrelated history.
```unison
unrelated1.foo = 100
unrelated2.foo = 200
```

```ucm
.> add

  ⍟ I've added these definitions:
  
    unrelated1.foo : Nat
      (also named source.ff1 and source2.ff1)
    unrelated2.foo : Nat
      (also named source2.ff2)

.> push.create transcripts.public.unrelated1 .unrelated1

  Uploaded 2 entities.

  View it on Unison Share: https://share.unison-lang.org/@transcripts/p/code/latest/namespaces/public/unrelated1

.> pull transcripts.public.unrelated1 .unrelated2

  Downloaded 0 entities.

  Merging...

  Applying changes from patch...

```
It looks like we don't have any guard against pulling an unrelated branch to a local location.
