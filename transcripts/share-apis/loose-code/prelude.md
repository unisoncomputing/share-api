```ucm:hide
.> project.create-empty transcripts
transcripts/main> builtins.mergeio
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
transcripts/main> add
```

```unison
-- Helpful for testing proper query encoding/decoding for a name that must be
-- uri encoded
a names./+% b = 10
```

```ucm
transcripts/main> add
transcripts/main> push
```
