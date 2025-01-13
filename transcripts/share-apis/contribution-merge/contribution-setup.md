Add a change to a contributor branch, based on the main branch.

```ucm:hide
scratch/main> clone @transcripts/merge/main
@transcripts/merge/main> branch @test/fast-forward-feature
```

```unison:hide
term = "feature"
```

Push the feature branch.

```ucm
@transcripts/merge/@test/fast-forward-feature> update
@transcripts/merge/@test/fast-forward-feature> push @transcripts/merge/@test/fast-forward-feature
```
