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
 :file (merge-pathnames "templates/asd.clt" (lem:lem-home)))
```

If you provide multiple templates for the same pattern, with the `:name` option (examples below), you will be prompted to choose a template.

You can create any kind of template you want in the [cl-template](https://github.com/alpha123/cl-template) format, `buffer` and `path` are passed to the template and you can read it with `(@ buffer)`, `(@ path)` etc.

Templating can be disabled if you put this in your config:

```lisp
(setf lem-template:*auto-template* nil)
```

## Many templates

We can use the plural `register-templates` to register multiple
templates at once, see examples below.

When more than one template is defined for the same file extension,
you will be prompted to choose.

Example, according we have those templates in `~/.config/lem/templates/`:

```lisp
(lem-template:register-templates
  (:pattern ".*\.asd"
   :name "Basic ASD"
   :file (merge-pathnames "templates/asd.clt" (lem-home)))
  (:pattern ".*\.asd"
   :name "Test ASD"
   :file (merge-pathnames "templates/test.asd.clt" (lem-home)))
  (:pattern ".*\.lisp"
   :file (merge-pathnames "templates/lisp.clt" (lem-home)))
   (:pattern ".*\.asd"
   :name "Lisp Makefile"
   :file (merge-pathnames "templates/Makefile.lisp.clt" (lem-home))))
```
