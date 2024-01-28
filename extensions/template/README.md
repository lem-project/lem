# lem-template

A templating extension to generate boilerplate in new files.

# Usage

Here is an example template file that generates a simple `.asd` to help get you started.

```
(asdf:defsystem "<%= (pathname-name (lem:buffer-filename (@ buffer))) %>"
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
  :template (merge-pathname "templates/asd.clt" (lem-home)))
```
