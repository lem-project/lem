;;;; Build the lem-terminal native helper (terminal.so) for the current
;;;; platform from extensions/terminal/terminal.c.
;;;;
;;;; Requires a C compiler and libvterm. On Linux, also requires libutil
;;;; (for forkpty); macOS provides it via libc.
;;;;
;;;; Invoke via `make terminal-lib` or directly:
;;;;   sbcl --script scripts/build-terminal.lisp
;;;;
;;;; The resulting .so is written under
;;;;   extensions/terminal/lib/<os>/<arch>/terminal.so
;;;; which matches the path lem-terminal/ffi.lisp pushes onto
;;;; cffi:*foreign-library-directories*.

(require :asdf)
(require :uiop)

(asdf:load-asd
 (merge-pathnames "extensions/terminal/lem-terminal.asd"
                  (uiop:getcwd)))

(defun terminal-source ()
  (asdf:system-relative-pathname :lem-terminal "terminal.c"))

(defun detect-architecture ()
  "Match ffi.lisp's runtime path resolution. Falls back to *features*
when uiop:architecture returns NIL (older bundled UIOP on Apple Silicon)."
  (or (uiop:architecture)
      (cond ((member :arm64 *features*) :arm64)
            ((member :aarch64 *features*) :arm64)
            ((member :x86-64 *features*) :x64)
            ((member :x86 *features*) :x86)
            (t (error "Could not detect architecture from *features*: ~A"
                      *features*)))))

(defun terminal-lib ()
  (asdf:system-relative-pathname
   :lem-terminal
   (format nil "~(lib/~A/~A/terminal.so~)"
           (uiop:operating-system)
           (detect-architecture))))

(defun build-command (source lib)
  (let ((cc (or (uiop:getenv "CC") "cc"))
        (cflags (or (uiop:getenv "CFLAGS") ""))
        (ldflags (or (uiop:getenv "LDFLAGS") "")))
    (format nil
            #+darwin "~A ~A ~A -I/opt/homebrew/include -L/opt/homebrew/lib -lvterm ~A -o ~A -shared -fPIC"
            #-darwin "~A ~A ~A -lvterm -lutil ~A -o ~A -shared -fPIC"
            cc cflags (namestring source) ldflags (namestring lib))))

(defun build-terminal ()
  (let* ((source (terminal-source))
         (lib (terminal-lib))
         (cmd (build-command source lib)))
    (unless (probe-file source)
      (error "terminal.c not found at ~A" source))
    (ensure-directories-exist lib)
    (format t "~&Building lem-terminal native helper:~%  ~A~%" cmd)
    (handler-case
        (progn
          (uiop:run-program cmd :output t :error-output t)
          (format t "~&Wrote ~A~%" lib))
      (uiop:subprocess-error (c)
        (format *error-output*
                "~&Failed to build terminal.so: ~A~%~
                 The lem-terminal extension will silently disable itself at~%~
                 runtime when the shared library is missing. Install libvterm~%~
                 (and a C toolchain) to enable it.~%"
                c)
        (uiop:quit 1)))))

(build-terminal)
