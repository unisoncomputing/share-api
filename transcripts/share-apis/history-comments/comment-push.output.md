``` ucm :hide
history-comments/main> builtins.mergeio lib.builtins
```

Add some history, then set comments on it.

``` unison :hide
x = 1
```

``` ucm
scratch/main> update

  Done.

scratch/main> config.set author.name Unison 

scratch/main> history.comment /main: "Initial commit with variable x set to 1"

  Done.

scratch/main> alias.term x y

  Done.

scratch/main> history.comment /main: "Renamed x to y"

  Done.

scratch/main> history

  Note: The most recent namespace hash is immediately below this
        message.

  ⊙ Unison
    ┃ Renamed x to y

  ⊙ 1. #tjd6qqlhod

    + Adds / updates:
    
      y
    
    = Copies:
    
      Original name New name(s)
      x             y


  ⊙ Unison
    ┃ Initial commit with variable x set to 1

  □ 2. #i52j9fd57b (start of history)

scratch/main> push @transcripts/history-comments/main

  Uploaded 5 entities.

  I just created @transcripts/history-comments on
  http://localhost:5424

  View it here: @transcripts/history-comments/main on http://localhost:5424
```
