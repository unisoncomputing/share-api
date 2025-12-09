``` ucm :hide
scratch/main> project.create-empty ambiguous

ambiguous/main> builtins.merge
```

Create a flawed component which should be rejected with an ambiguous element ordering error.

``` unison
ping = do
  pong()

pong = do
  ping()
```

``` ucm :added-by-ucm
  Loading changes detected in scratch.u.

  + ping : 'r
  + pong : 'r

  Run `update` to apply these changes to your codebase.
```

``` ucm :hide
ambiguous/main> update
```

Try to push, this should be rejected.

``` ucm
ambiguous/main> push @transcripts/ambiguous/main
```

🛑

The transcript failed due to an error in the stanza above. The error is:

``` 
❗️

Failed to decode a term component entity with the hash
#6nhsou2jbd9o95ftlp83v1dhqo7cmk0jb4tgkecmurku9en8qqnr205arlacgj3tumu6sfudhi100lqcvu2sbf4t412nngvbeuboeig
.
Please create an issue and report this to the Unison team

The error was: Incomplete element ordering in term components
```
