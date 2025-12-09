```ucm:hide
scratch/main> project.create-empty ambiguous
ambiguous/main> builtins.merge
```

Create a flawed component which should be rejected with an ambiguous element ordering error.

```unison
ping = do
  pong()

pong = do
  ping()
```


```ucm:hide
ambiguous/main> update
```

Try to push, this should be rejected.
```ucm
ambiguous/main> push @transcripts/ambiguous/main
```
