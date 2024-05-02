```ucm:hide
.> project.create-empty namespace-diff
namespace-diff/main> branch /diff-start
namespace-diff/diff-start> builtins.mergeio
```

```unison
termLeaveMeAlone = 0
termAliasMe = 1
termDeleteMe = 2
termRenameMe = 3
termUpdateMe = "original"

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
```

```ucm:hide
namespace-diff/diff-start> add
namespace-diff/diff-start> push @transcripts/namespace-diff/diff-start
namespace-diff/diff-start> branch /diff-end
namespace-diff/diff-end> alias.term termAliasMe aTermAlias
namespace-diff/diff-end> delete.term termDeleteMe
namespace-diff/diff-end> alias.type DataAliasMe ATypeAlias
namespace-diff/diff-end> delete.type DataDeleteMe
namespace-diff/diff-end> delete.namespace DataDeleteMe
namespace-diff/diff-end> alias.type AbilityAliasMe AbilityAlias
namespace-diff/diff-end> delete.type AbilityDeleteMe
namespace-diff/diff-end> delete.namespace AbilityDeleteMe
namespace-diff/diff-end> delete.term a.definition.at.path1
namespace-diff/diff-end> delete.term a.definition.at.path2
namespace-diff/diff-end> delete.term a.different.path
namespace-diff/diff-end> delete.term aDoc
namespace-diff/diff-end> delete.term aTest
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
```

```ucm
namespace-diff/diff-end> update
namespace-diff/diff-end> rename.term termRenameMe renamedTerm
namespace-diff/diff-end> rename.type DataRenameMe RenamedType
namespace-diff/diff-end> rename.namespace DataRenameMe RenamedType
namespace-diff/diff-end> rename.type AbilityRenameMe AbilityRenamed
namespace-diff/diff-end> rename.namespace AbilityRenameMe AbilityRenamed
namespace-diff/diff-end> push @transcripts/namespace-diff/diff-end
```

Now we go back to the `diff-start` branch and make some more commits to test that 
diffing uses the best common ancestor.

```unison
addedOnDiffStartAfterFork = "added on diff-start after fork"

-- Add a conflicting update.
termUpdateMe = "conflicted update"
```

```ucm
namespace-diff/diff-start> update
namespace-diff/diff-start> delete.term deleteMeAfterFork
namespace-diff/diff-start> push @transcripts/namespace-diff/diff-start
```
