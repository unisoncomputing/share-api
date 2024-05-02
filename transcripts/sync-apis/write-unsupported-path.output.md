Currently, only the `public namespace is writeable.
```ucm
.path> push.create transcripts.notpublic

  ❗️
  
  Unison Share currently only supports sharing public code. This
  is done by hosting code in a public namespace under your
  handle. It looks like you were trying to push directly to the
  `transcripts` handle. Try nesting under `public` like so:
  
      push.create transcripts.public.notpublic

```
