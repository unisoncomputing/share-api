```unison
x = 1
```

Create a project, add some history, then push it.

```ucm
scratch/main> project.create-empty myproject
myproject/main> update
myproject/main> move.term x y
myproject/main> move.term y z
myproject/main> history
myproject/main> push
myproject/main> branch.create-empty dest
myproject/dest> pull.without-history @transcripts/myproject/main
myproject/dest> history
```
