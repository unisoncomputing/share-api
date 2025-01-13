## Best Common Ancestor updates

In order to provide relevant diffs, Share finds the best common ancestor of the two branches being compared. 
I.e. The most recent commit that is an ancestor of both branches.

Let's set up a main branch with two feature branches: 

`main <- feature-one <- feature-two`

```ucm:hide
scratch/main> project.create-empty bca-updates
bca-updates/main>
```

```unison:hide
term = "start"
```

```ucm
bca-updates/main> add
bca-updates/main> push @transcripts/bca-updates/main
bca-updates/main> branch /feature-one
```

```unison:hide
term = "feature-one"
```

```ucm
bca-updates/feature-one> update
bca-updates/feature-one> push @transcripts/bca-updates/feature-one
bca-updates/feature-one> branch /feature-two
```

```unison:hide
term = "feature-two"
```

```ucm
bca-updates/feature-two> update
bca-updates/feature-two> push @transcripts/bca-updates/feature-two
```

See the script, we will create a contribution, then will merge and push branches to see how things change.
