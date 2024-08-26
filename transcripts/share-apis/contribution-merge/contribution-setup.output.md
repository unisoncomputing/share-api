Create a main branch.

``` unison
term = "start"
```

Push it.

``` ucm
merge/main> add

  ⍟ I've added these definitions:
  
    term : ##Text

merge/main> push @transcripts/merge/main

  Uploaded 3 entities.

  I just created @transcripts/merge on http://localhost:5424

  View it here: @transcripts/merge/main on http://localhost:5424

merge/main> branch /fast-forward-feature

  Done. I've created the fast-forward-feature branch based off
  of main.
  
  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /fast-forward-feature`.

```
Add a change to a feature branch, based on the main branch.

``` unison
term = "feature"
```

Push the feature branch.

``` ucm
merge/fast-forward-feature> update

  Okay, I'm searching the branch for code that needs to be
  updated...

  Done.

merge/fast-forward-feature> push @transcripts/merge/fast-forward-feature

  Uploaded 3 entities.

  I just created @transcripts/merge/fast-forward-feature on
  http://localhost:5424

  View it here: @transcripts/merge/fast-forward-feature on http://localhost:5424

```
