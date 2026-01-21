```ucm:hide
scratch/main> builtins.mergeio
scratch/main> project.create-empty code-browse
code-browse/main> builtins.mergeio
```

Set up some stuff to pull in as a lib.

```unison
structural type data.Tuple a b = Tuple a b

data.Tuple.fst = cases
  Tuple a _ -> a

data.Tuple.snd = cases
  Tuple _ b -> b

-- We shouldn't qualify project names if the only conflict is in lib.
data.myName = "libName"

-- We _should_ still qualify names in lib if all conflicts are in libs.
other.myOtherName = "other.myOtherName"
data.myOtherName = "data.myOtherName"
```

```ucm
mylib/main> update
code-browse/main> lib.install.local mylib/main mylib
```

Set up some shared names so we can do interesting transcript tests

```unison
structural type names.Thing = This | That

structural type names.WithDependencies = WithDependencies (lib.mylib.data.Tuple names.Thing names.Thing)

names.apples.compoundTerm = 1 + names.apples.two
names.apples.two = 2
names.referencesExternal = 10 + names.apples.two + external.two
names.oranges.two = 2
names.readme = {{ Hello! }}
external.two = 2
external.externalName = 99

-- Helpful for testing proper query encoding/decoding for a name that must be
-- uri encoded
a names./+% b = 10


-- Test that suffixification works as expected

ns.myName = "projectName"


-- A term which references the names we defined above
macroTerm = do
  _ = ns.myName
  _ = lib.mylib.data.myName
  _ = lib.mylib.other.myOtherName
  1
```

```ucm
code-browse/main> update
code-browse/main> names names.referencesExternal
code-browse/main> names names.WithDependencies
code-browse/main> names names.Thing
code-browse/main> names names.apples.two
code-browse/main> push
```
