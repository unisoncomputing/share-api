```ucm:hide
scratch/main> project.create-empty contribution-diff
contribution-diff/main> branch /diff-start
contribution-diff/diff-start> builtins.mergeio
```

```unison
termLeaveMeAlone = 0
termAliasMe = 1
termDeleteMe = 2
termRenameMe = 3
termUpdateMe = "original"
termDependsOnUpdateMe = termUpdateMe ++ termUpdateMe

type DataLeaveMeAlone = A
type DataAliasMe = B
type DataDeleteMe = C
type DataUpdateMe = D
type DataRenameMe = E


ability AbilityLeaveMeAlone where
   abilityLeaveMeAlone : Nat

ability AbilityAliasMe where
   abilityAliasMe : Nat

ability AbilityDeleteMe where
    abilityDeleteMe : Nat

ability AbilityUpdateMe where
    abilityUpdateMe : Nat

ability AbilityRenameMe where
    abilityRenameMe : Nat


-- Test path compression in output diff.
a.definition.at.path1 = "definition at path"
a.definition.at.path2 = "definition at path2"
a.different.path = "definition at different path"

-- Ensure defn tag detection is correct.
aDoc = {{ Test Doc }}

aTest = [Ok "Done"]

deleteMeAfterFork = "delete me after fork"

type MyType = { x : Nat }
```

```ucm:hide
contribution-diff/diff-start> update
contribution-diff/diff-start> push @transcripts/contribution-diff/diff-start
contribution-diff/diff-start> branch /diff-end
contribution-diff/diff-end> alias.term termAliasMe aTermAlias
contribution-diff/diff-end> delete.term termDeleteMe
contribution-diff/diff-end> alias.type DataAliasMe ATypeAlias
contribution-diff/diff-end> delete.type DataDeleteMe
contribution-diff/diff-end> alias.type AbilityAliasMe AbilityAlias
contribution-diff/diff-end> delete.type AbilityDeleteMe
contribution-diff/diff-end> delete.term a.definition.at.path1
contribution-diff/diff-end> delete.term a.definition.at.path2
contribution-diff/diff-end> delete.term a.different.path
contribution-diff/diff-end> delete.term aDoc
contribution-diff/diff-end> delete.term aTest
```

```unison
newTerm = 100

termUpdateMe = "updated"

type DataUpdateMe = D2 Nat

type NewType = X

ability AbilityUpdateMe where
    abilityUpdateMe : Text

ability AbilityNew where
    abilityNew : Text

type MyType = { x : Nat, newFieldWithAReallyReallyReallyReallyReallyLongNameThatWillForceThePrettyPrinterToWrapItIHope : Text, y : Boolean}
```

```ucm
contribution-diff/diff-end> update
contribution-diff/diff-end> rename.term termRenameMe renamedTerm
contribution-diff/diff-end> rename.type DataRenameMe RenamedType
contribution-diff/diff-end> rename.namespace DataRenameMe RenamedType
contribution-diff/diff-end> rename.type AbilityRenameMe AbilityRenamed
contribution-diff/diff-end> rename.namespace AbilityRenameMe AbilityRenamed
contribution-diff/diff-end> push @transcripts/contribution-diff/diff-end
```

Now we go back to the `diff-start` branch and make some more commits to test that
diffing uses the best common ancestor.

```unison
addedOnDiffStartAfterFork = "added on diff-start after fork"

-- Add a conflicting update.
termUpdateMe = "conflicted update"
```

```ucm
contribution-diff/diff-start> update
contribution-diff/diff-start> delete.term deleteMeAfterFork
contribution-diff/diff-start> push @transcripts/contribution-diff/diff-start
```
