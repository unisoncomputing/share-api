```ucm
.> builtins.merge
```

Add a bunch of nonsense types and terms so we can test search.

We use structural types and abilities.

```unison
structural type Remote.Location = Location

structural ability Remote where
  at : Remote.Location -> ()

structural type MyType = MyType

other.remoteMap = 1
something.Remote.map = 2
some.other.thing = 3
```


```ucm
.> add
```

```ucm
.> push @transcripts/branch-browse/main
```

Update a few things so we can verify that pushing to an existing branch works.

```unison
other.remoteMap = 10
something.Remote.map = 20
some.other.thing = 30
```

```ucm
.> update
.> history
.> push @transcripts/branch-browse/main
```
