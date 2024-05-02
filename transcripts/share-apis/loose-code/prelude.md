```ucm:hide
.> builtins.mergeio
```

Set up some shared names so we can do interesting transcript tests

```unison
structural type names.Thing = This | That

names.apples.compoundTerm = 1 + names.apples.two
names.apples.two = 2
names.referencesExternal = 10 + names.apples.two + external.two
names.oranges.two = 2
names.readme = {{ Hello! }}
external.two = 2
external.externalName = 99
```


```ucm
.> add
```

```unison
-- Helpful for testing proper query encoding/decoding for a name that must be
-- uri encoded
a /+% b = 10
```

```ucm
.names> add
.> push.create transcripts.public
```
