```ucm
transitiveDep/main> builtins.mergeio
dep/main> builtins.mergeio
app/main> builtins.mergeio
```

```unison
transitiveDepNum = 1
```

```ucm
transitiveDep/main> update
dep/main> lib.install.local transitiveDep/main transitiveDep
```

```unison
depNum = lib.transitiveDep.transitiveDepNum
```

```ucm
dep/main> update
app/main> lib.install.local dep/main dep
```

```unison
-- Definitions can depend on dependencies
someTerm = lib.dep.depNum

README = {{
## Title

References {someTerm}
}}
```

```ucm
app/main> update
app/main> push
```
