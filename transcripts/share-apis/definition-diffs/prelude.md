## Definition Diffs

Uses state from above as well 👆

```ucm:hide
.> project.create-empty definition-diff
definition-diff/main> branch /before
definition-diff/before> builtins.mergeio
```

```unison
valueChangesButNameStaysSame = 0
nameChangesButValueStaysSame = 10
beforeTerm x y = do
  myList = [x, y, valueChangesButNameStaysSame, nameChangesButValueStaysSame]
  myList2 = List.map (x -> x + 1) myList
  size myList2
type BeforeType = { 
  fieldOne : Nat
  }
ability BeforeAbility where
  beforeAbility : Nat -> Text
```

```ucm:hide
definition-diff/before> add
definition-diff/before> push @transcripts/definition-diff/before
definition-diff/before> branch.create-empty /after
definition-diff/after> builtins.mergeio
```

```unison
valueChangesButNameStaysSame = 1
changedNameWithSameValue = 10
afterTerm x y z = do
  myList = [x, y, valueChangesButNameStaysSame, changedNameWithSameValue]
  myList2 = List.map (x -> x + z) myList
  size myList2
newBug = "No longer a builtin"
type NewIO = NewIO
type AfterType = { 
  fieldOne : Nat, 
  fieldTwo : Text
  }
ability AfterAbility where
  afterAbility : Int -> Text
```

```ucm
definition-diff/after> update
definition-diff/after> push @transcripts/definition-diff/after
```
