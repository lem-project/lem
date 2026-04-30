# Frontend Development

## Interface Protocol

Frontends subclass `implementation` and implement `lem-if:*` generics.

## Required Methods

```lisp
;; Core
(lem-if:invoke impl function)           ; Start event loop
(lem-if:display-width impl)             ; Screen width (chars)
(lem-if:display-height impl)            ; Screen height (chars)
(lem-if:update-display impl)            ; Flush to screen

;; Views
(lem-if:make-view impl window x y w h use-modeline)
(lem-if:delete-view impl view)
(lem-if:set-view-size impl view w h)
(lem-if:set-view-pos impl view x y)

;; Rendering
(lem-if:render-line impl view x y objects height)
(lem-if:clear-to-end-of-window impl view y)
```

## Implementation Capabilities

Set in class definition:

```lisp
(defclass my-impl (implementation)
  ()
  (:default-initargs
   :name :my-frontend
   :support-floating-window t
   :window-left-margin 1))
```

## Event Handling

Send events to core via `send-event`:

```lisp
(send-event (lambda () (execute-key-sequence keys)))
```

## Reference

- `frontends/sdl2/` - Full-featured GUI
- `frontends/ncurses/` - Terminal
- `frontends/fake-interface/` - Testing stub
