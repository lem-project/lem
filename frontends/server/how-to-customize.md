# how to customize

## font
```lisp
(set-font :name "Ubuntu Sans Mono" :size 19)
```

## floating window

This is a temporary API and is likely to change in the future.

```lisp
(lem-server::load-css ".lem-editor__floating-window--bordered {
  all: unset;
  border: 1px solid white;
  background-color: white;
}")
```
