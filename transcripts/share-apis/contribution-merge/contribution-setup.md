```ucm:hide
scratch/main> project.create-empty merge
```

Create a main branch.

```unison:hide
term = "start"
```

Push it.

```ucm
merge/main> add
merge/main> push @transcripts/merge/main
merge/main> branch /fast-forward-feature
```

Add a change to a feature branch, based on the main branch.

```unison:hide
term = "feature"
```

Push the feature branch.

```ucm
merge/fast-forward-feature> update
merge/fast-forward-feature> push @transcripts/merge/fast-forward-feature
```
