# lem-template

A templating extension to generate boilerplate in new files.

# Usage

Here is an example template file that generates a simple `.asd` to help get you started.

```
(asdf:defsystem "<%= (pathname-name (@ path)) %>"
  :author ""
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components ((:file "core")))))
```

Assuming this file exists in `~/.config/lem/templates/asd.clt`, you can register it like this:

```lisp
(lem-template:register-template
 :pattern ".*\.asd"
 :file (merge-pathnames "templates/asd.clt" (lem-home)))
```

You can create any kind of template you want in the [cl-template](https://github.com/alpha123/cl-template) format, `buffer` and `path` are passed to the template and you can read it with `(@ buffer)` etc.

# Examples

See [my templates](https://github.com/garlic0x1/.lem/tree/master/templates) for more examples, I used the plural `register-templates` to register them like this:

```lisp
(register-templates
  (:pattern ".*\.asd"  :file (merge-pathnames "templates/asd.clt"      (lem-home)))
  (:pattern ".*\.lisp" :file (merge-pathnames "templates/lisp.clt"     (lem-home)))
  (:pattern ".*\.go"   :file (merge-pathnames "templates/go.clt"       (lem-home)))
  (:pattern "Makefile" :file (merge-pathnames "templates/Makefile.clt" (lem-home))))
```
