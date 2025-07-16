## Best Common Ancestor updates

In order to provide relevant diffs, Share finds the best common ancestor of the two branches being compared.
I.e. The most recent commit that is an ancestor of both branches.

Let's set up a main branch with two feature branches:

`main <- feature-one <- feature-two`

``` ucm :hide
scratch/main> project.create-empty bca-updates
```

``` unison :hide
term = "start"
```

``` ucm
bca-updates/main> update

  Done.

bca-updates/main> push @transcripts/bca-updates/main

  Uploaded 3 entities.

  I just created @transcripts/bca-updates on
  http://localhost:5424

  View it here: @transcripts/bca-updates/main on http://localhost:5424

bca-updates/main> branch /feature-one

  Done. I've created the feature-one branch based off of main.

  Tip: To merge your work back into the main branch, first
       `switch /main` then `merge /feature-one`.
```

``` unison :hide
term = "feature-one"
```

``` ucm
bca-updates/feature-one> update

  Done.

bca-updates/feature-one> push @transcripts/bca-updates/feature-one

  Uploaded 3 entities.

  I just created @transcripts/bca-updates/feature-one on
  http://localhost:5424

  View it here: @transcripts/bca-updates/feature-one on http://localhost:5424

bca-updates/feature-one> branch /feature-two

  Done. I've created the feature-two branch based off of
  feature-one.

  Tip: To merge your work back into the feature-one branch,
       first `switch /feature-one` then `merge /feature-two`.
```

``` unison :hide
term = "feature-two"
```

``` ucm
bca-updates/feature-two> update

  Done.

bca-updates/feature-two> push @transcripts/bca-updates/feature-two

  Uploaded 3 entities.

  I just created @transcripts/bca-updates/feature-two on
  http://localhost:5424

  View it here: @transcripts/bca-updates/feature-two on http://localhost:5424
```

See the script, we will create a contribution, then will merge and push branches to see how things change.
