```ucm
.> clone @transcripts/bca-updates/main

  Downloaded 3 entities.

  Cloned @transcripts/bca-updates/main.

.> clone @transcripts/bca-updates/feature-one

  Downloaded 3 entities.

  Cloned @transcripts/bca-updates/feature-one.

-- Merge the feature branch, then push the merged branch to main
@transcripts/bca-updates/main> merge /feature-one

  Here's what's changed in the current namespace after the
  merge:
  
  Updates:
  
    1. term : ##Text
       ↓
    2. term : ##Text
  
  Tip: You can use `todo` to see if this generated any work to
       do in this namespace and `test` to run the tests. Or you
       can use `undo` or `reflog` to undo the results of this
       merge.

  Applying changes from patch...

@transcripts/bca-updates/main> push @transcripts/bca-updates/main

  Uploaded 1 entities.

  View it on Unison Share: @transcripts/bca-updates/main on http://localhost:5424

```
