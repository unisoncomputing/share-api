``` unison
x = 1
```

``` ucm

  Loading changes detected in scratch.u.

  I found and typechecked these definitions in scratch.u. If you
  do an `add` or `update`, here's how your codebase would
  change:
  
    ⍟ These new definitions are ok to `add`:
    
      x : ##Nat

```
Create a project, add some history, then push it.

``` ucm
scratch/main> project.create-empty myproject

  🎉 I've created the project myproject.

  🎨 Type `ui` to explore this project's code in your browser.
  🔭 Discover libraries at https://share.unison-lang.org
  📖 Use `help-topic projects` to learn more about projects.
  
  Write your first Unison code with UCM:
  
    1. Open scratch.u.
    2. Write some Unison code and save the file.
    3. In UCM, type `add` to save it to your new project.
  
  🎉 🥳 Happy coding!

myproject/main> add

  ⍟ I've added these definitions:
  
    x : ##Nat

myproject/main> move.term x y

  Done.

myproject/main> move.term y z

  Done.

myproject/main> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  ⊙ 1. #leqcfs7jl2
  
    > Moves:
    
      Original name New name
      y             z
  
  ⊙ 2. #8p3ptealk8
  
    > Moves:
    
      Original name New name
      x             y
  
  □ 3. #i52j9fd57b (start of history)

myproject/main> push

  Uploaded 7 entities.

  I just created @transcripts/myproject on http://localhost:5424

  View it here: @transcripts/myproject/main on http://localhost:5424

myproject/main> branch.create-empty dest

  Done. I've created an empty branch myproject/dest.
  
  Tip: Use `merge /somebranch` to initialize this branch.

myproject/dest> pull.without-history @transcripts/myproject/main

  Downloaded 1 entities.

  ✅
  
  Successfully updated myproject/dest from
  @transcripts/myproject/main.

myproject/dest> history

  Note: The most recent namespace hash is immediately below this
        message.
  
  
  
  □ 1. #gd3audeh9u (start of history)

```
