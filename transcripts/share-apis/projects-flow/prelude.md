```unison
-- Dependencies depend on transitive dependencies
lib.someLib.depNum = lib.dep.lib.transitiveDep.transitiveDepNum
lib.dep.lib.transitiveDep.transitiveDepNum = 1

-- Definitions can depend on dependencies
someTerm = lib.someLib.depNum
```

```ucm
.> project.create-empty transcriptproject
transcriptproject/main> add
transcriptproject/main> push
```
