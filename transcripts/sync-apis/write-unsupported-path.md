```unison:hide:all
stuff = 300
```

```ucm:hide
.path> add
```

Currently, only the `public namespace is writeable.
```ucm:error
.path> push.create transcripts.notpublic
```
