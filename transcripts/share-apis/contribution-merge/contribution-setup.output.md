Add a change to a contributor branch, based on the main branch.

``` ucm :hide
scratch/main> clone @transcripts/merge/main

@transcripts/merge/main> branch @test/fast-forward-feature
```

``` unison :hide
term = "feature"
```

Push the feature branch.

``` ucm
@transcripts/merge/@test/fast-forward-feature> update

  Done.

@transcripts/merge/@test/fast-forward-feature> push @transcripts/merge/@test/fast-forward-feature

  Uploaded 3 entities.

  I just created @transcripts/merge/@test/fast-forward-feature
  on http://localhost:5424

  View it here: @transcripts/merge/@test/fast-forward-feature on http://localhost:5424
```
