

```ucm:hide
@transcripts/escaped-names/main> builtins.mergeio
```

Add definitions with escaped symboly names, and docs so we have namespaces with
escaped symboly names too.

```unison
-- A definition named `.`, this is a regression test since there are old definitions named `.`
-- in the history of some code on Share.
{{ Dot-compose!
}}
g `.` f = a -> g (f a)

{{ What's this do? Who knows, but the name is cool! }}
x `.!~` y = x `.` y
```

```ucm
@transcripts/escaped-names/main> update
@transcripts/escaped-names/main> push @transcripts/escaped-names/main
```
