```unison
x = 1
```

Create a project, add some history, then push it.

```ucm
.> project.create-empty myproject
myproject/main> add
myproject/main> move.term x y
myproject/main> move.term y z
myproject/main> history
myproject/main> push
.dest> pull.without-history @transcripts/myproject/main
.dest> history
```
