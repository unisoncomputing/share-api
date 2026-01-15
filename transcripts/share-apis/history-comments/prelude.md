```ucm:hide
history-comments/main> builtins.mergeio lib.builtins
```

Add some history, then set comments on it.

```unison:hide
x = 1
```

```ucm
scratch/main> config.set author.name Unison 
scratch/main> history.comment /main: "Initial commit with variable x set to 1"
scratch/main> alias.term x y
scratch/main> history.comment /main: "Renamed x to y"
scratch/main> history
scratch/main> push @test/history-comments/main
```
