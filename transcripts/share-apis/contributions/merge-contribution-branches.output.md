``` ucm
scratch/main> clone @transcripts/bca-updates/main

  Downloaded 3 entities.

  Cloned @transcripts/bca-updates/main.

scratch/main> clone @transcripts/bca-updates/feature-one

  Downloaded 3 entities.

  Cloned @transcripts/bca-updates/feature-one.

-- Merge the feature branch, then push the merged branch to main

@transcripts/bca-updates/main> merge /feature-one

  I fast-forward merged @transcripts/bca-updates/feature-one
  into @transcripts/bca-updates/main.

@transcripts/bca-updates/main> push @transcripts/bca-updates/main

  Uploaded 1 entities.

  View it here: @transcripts/bca-updates/main on http://localhost:5424
```
