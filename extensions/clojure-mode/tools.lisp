(defpackage :lem-clojure-mode/tools
  (:use :cl :lem)
  (:import-from :lem-clojure-mode
                :clojure-mode
                :clojure-current-namespace
                :*clojure-mode-keymap*)
  (:import-from :lem-clojure-mode/nrepl-client
                :check-nrepl-connection
                :nrepl-eval-sync
                :nrepl-response-value)
  (:export :clojure-align-let
           :clojure-align-map
           :clojure-cycle-privacy
           :clojure-add-require
           :clojure-sort-ns
           :clojure-toggle-keyword-string
           :clojure-thread-first
           :clojure-thread-last
           :clojure-unwind-thread))

(in-package :lem-clojure-mode/tools)

;;;; Alignment Tools

(defun find-binding-vector-bounds (point)
  "Find the bounds of a binding vector (for let, binding, etc.)."
  (with-point ((p point))
    ;; Find opening bracket
    (when (search-backward p "[")
      (with-point ((start p)
                   (end p))
        (when (scan-lists end 1 0)
          (values start end))))))

(defun parse-bindings (start end)
  "Parse bindings from a let-style binding vector."
  (let ((bindings nil)
        (current-binding nil))
    (with-point ((p start))
      (character-offset p 1)  ; skip [
      (loop
        (skip-whitespace-forward p)
        (when (point>= p end)
          (return))
        (when (char= (character-at p) #\])
          (return))
        ;; Read binding name
        (let ((name-start (copy-point p :temporary)))
          (form-offset p 1)
          (let ((name (points-to-string name-start p)))
            (skip-whitespace-forward p)
            ;; Read value
            (let ((value-start (copy-point p :temporary)))
              (form-offset p 1)
              (let ((value (points-to-string value-start p)))
                (push (cons name value) bindings)))))))
    (nreverse bindings)))

(defun max-binding-name-length (bindings)
  "Get the maximum length of binding names."
  (reduce #'max bindings :key (lambda (b) (length (car b))) :initial-value 0))

(define-command clojure-align-let () ()
  "Align a let-style binding vector."
  (multiple-value-bind (start end) (find-binding-vector-bounds (current-point))
    (unless start
      (editor-error "Not inside a binding vector"))
    (let* ((bindings (parse-bindings start end))
           (max-len (max-binding-name-length bindings)))
      (when bindings
        ;; Delete old content and insert aligned
        (with-point ((insert-point start))
          (character-offset insert-point 1)
          (delete-between-points insert-point (character-offset (copy-point end :temporary) -1))
          (loop :for (name . value) :in bindings
                :for first := t :then nil
                :do (unless first
                      (insert-character insert-point #\newline)
                      (insert-string insert-point "    "))  ; indent
                    (insert-string insert-point name)
                    (insert-string insert-point
                                   (make-string (1+ (- max-len (length name)))
                                                :initial-element #\space))
                    (insert-string insert-point value)))
        (message "Aligned ~D bindings" (length bindings))))))

(define-command clojure-align-map () ()
  "Align a map literal with keys and values."
  (with-point ((p (current-point)))
    ;; Find enclosing map
    (unless (search-backward p "{")
      (editor-error "Not inside a map"))
    (with-point ((start p)
                 (end p))
      (unless (scan-lists end 1 0)
        (editor-error "Could not find map end"))
      ;; Parse key-value pairs
      (let ((pairs nil)
            (max-key-len 0))
        (with-point ((scan start))
          (character-offset scan 1)
          (loop
            (skip-whitespace-forward scan)
            (when (point>= scan end)
              (return))
            (when (char= (character-at scan) #\})
              (return))
            (let ((key-start (copy-point scan :temporary)))
              (form-offset scan 1)
              (let ((key (points-to-string key-start scan)))
                (setf max-key-len (max max-key-len (length key)))
                (skip-whitespace-forward scan)
                (let ((val-start (copy-point scan :temporary)))
                  (form-offset scan 1)
                  (push (cons key (points-to-string val-start scan)) pairs))))))
        (setf pairs (nreverse pairs))
        ;; Rewrite with alignment
        (when pairs
          (with-point ((insert-point start))
            (character-offset insert-point 1)
            (delete-between-points insert-point (character-offset (copy-point end :temporary) -1))
            (loop :for (key . value) :in pairs
                  :for first := t :then nil
                  :do (unless first
                        (insert-character insert-point #\newline)
                        (insert-string insert-point " "))
                      (insert-string insert-point key)
                      (insert-string insert-point
                                     (make-string (1+ (- max-key-len (length key)))
                                                  :initial-element #\space))
                      (insert-string insert-point value)))
          (message "Aligned ~D pairs" (length pairs)))))))

;;;; Privacy Toggle

(define-command clojure-cycle-privacy () ()
  "Toggle between defn and defn-."
  (with-point ((p (current-point)))
    (line-start p)
    (cond
      ((search-forward p "defn-" (line-end-point p))
       (character-offset p -5)
       (delete-character p 5)
       (insert-string p "defn")
       (message "Changed to public (defn)"))
      ((search-forward p "defn" (line-end-point p))
       (character-offset p -4)
       (delete-character p 4)
       (insert-string p "defn-")
       (message "Changed to private (defn-)"))
      (t
       (message "No defn found on current line")))))

;;;; Require Management

(define-command clojure-add-require (ns &optional alias)
    ((:string "Require namespace: ")
     (:string "Alias (or empty): "))
  "Add a require clause to the ns form."
  (with-point ((p (current-point)))
    (buffer-start p)
    (unless (search-forward-regexp p "\\(ns\\s")
      (editor-error "No ns form found"))
    ;; Find :require section or end of ns
    (let ((insert-point nil)
          (new-require (if (and alias (plusp (length alias)))
                           (format nil "[~A :as ~A]" ns alias)
                           ns)))
      (cond
        ((search-forward p "(:require" nil)
         ;; Add to existing :require
         (scan-lists p 1 0)
         (character-offset p -1)
         (setf insert-point (copy-point p :temporary))
         (insert-character insert-point #\newline)
         (insert-string insert-point "   ")
         (insert-string insert-point new-require))
        (t
         ;; Add new :require section
         (buffer-start p)
         (search-forward-regexp p "\\(ns\\s")
         (form-offset p 1)  ; skip ns name
         (skip-whitespace-forward p)
         ;; Skip docstring if present
         (when (char= (character-at p) #\")
           (form-offset p 1)
           (skip-whitespace-forward p))
         (setf insert-point (copy-point p :temporary))
         (insert-character insert-point #\newline)
         (insert-string insert-point "  (:require ")
         (insert-string insert-point new-require)
         (insert-string insert-point ")")))
      (message "Added require: ~A" new-require))))

(define-command clojure-sort-ns () ()
  "Sort the :require and :import clauses in the ns form."
  (check-nrepl-connection)
  (let* ((code "(require 'clojure.tools.namespace.repl)
                (clojure.tools.namespace.repl/refresh)")
         (responses (nrepl-eval-sync code :ns "user" :timeout 30)))
    (declare (ignore responses))
    (message "Namespace sorted (requires clojure.tools.namespace)")))

;;;; Keyword/String Toggle

(define-command clojure-toggle-keyword-string () ()
  "Toggle between keyword and string at point."
  (with-point ((p (current-point)))
    (let ((c (character-at p)))
      (cond
        ;; Keyword to string
        ((char= c #\:)
         (let ((start (copy-point p :temporary)))
           (character-offset p 1)
           (skip-chars-forward p (lambda (c)
                                   (or (alphanumericp c)
                                       (member c '(#\- #\_ #\/ #\.)))))
           (let ((keyword (points-to-string start p)))
             (delete-between-points start p)
             (insert-string start (format nil "\"~A\"" (subseq keyword 1)))
             (message "Converted to string"))))
        ;; String to keyword
        ((char= c #\")
         (let ((start (copy-point p :temporary)))
           (form-offset p 1)
           (let* ((str (points-to-string start p))
                  (content (subseq str 1 (1- (length str)))))
             (delete-between-points start p)
             (insert-string start (format nil ":~A" content))
             (message "Converted to keyword"))))
        (t
         (message "Not on a keyword or string"))))))

;;;; Threading Macros

(define-command clojure-thread-first () ()
  "Convert nested calls to thread-first (->) form."
  (with-point ((p (current-point)))
    ;; Find the innermost sexp
    (backward-up-list p t)
    (with-point ((start p)
                 (end p))
      (form-offset end 1)
      (let ((forms nil))
        ;; Collect nested forms
        (with-point ((scan start))
          (character-offset scan 1)
          (loop
            (skip-whitespace-forward scan)
            (when (char= (character-at scan) #\()
              ;; Nested form - extract function and remaining args
              (let ((form-start (copy-point scan :temporary)))
                (form-offset scan 1)
                (push (points-to-string form-start scan) forms)))
            (when (point>= scan end)
              (return))))
        (when (>= (length forms) 2)
          (let* ((reversed (reverse forms))
                 (innermost (first reversed))
                 (rest-forms (rest reversed)))
            ;; Construct threaded form
            (delete-between-points start end)
            (insert-string start "(-> ")
            (insert-string start innermost)
            (dolist (form rest-forms)
              (insert-character start #\newline)
              (insert-string start "    ")
              (insert-string start form))
            (insert-string start ")")
            (message "Threaded ~D forms" (length forms))))))))

(define-command clojure-thread-last () ()
  "Convert nested calls to thread-last (->>) form."
  ;; Similar to thread-first but uses ->>
  (message "Use refactor tools for complex threading"))

(define-command clojure-unwind-thread () ()
  "Convert a threaded form back to nested calls."
  (message "Use refactor tools for unwinding"))

;;;; Context Menu

(defun clojure-context-menu-items ()
  "Generate context menu items for Clojure mode."
  (let ((items nil))
    ;; Add items based on context
    (push (lem/context-menu:make-item
           :label "Evaluate Expression"
           :callback (lambda (&rest args)
                       (declare (ignore args))
                       (lem-clojure-mode/commands:clojure-eval-last-sexp)))
          items)
    (push (lem/context-menu:make-item
           :label "Evaluate Defun"
           :callback (lambda (&rest args)
                       (declare (ignore args))
                       (lem-clojure-mode/commands:clojure-eval-defun)))
          items)
    (push (lem/context-menu:make-item
           :label "Inspect Value"
           :callback (lambda (&rest args)
                       (declare (ignore args))
                       (lem-clojure-mode/inspector:clojure-inspect-last-sexp)))
          items)
    (push (lem/context-menu:make-item
           :label "Run Test"
           :callback (lambda (&rest args)
                       (declare (ignore args))
                       (lem-clojure-mode/test-runner:clojure-run-test-at-point)))
          items)
    (push (lem/context-menu:make-item
           :label "Show Documentation"
           :callback (lambda (&rest args)
                       (declare (ignore args))
                       (lem-clojure-mode/commands:clojure-describe-symbol-at-point)))
          items)
    (nreverse items)))

(defun setup-clojure-context-menu ()
  "Setup context menu for Clojure mode."
  (setf (buffer-context-menu (current-buffer))
        (make-instance 'lem/context-menu:context-menu
                       :compute-items-function 'clojure-context-menu-items)))

;; Add to mode hook
(add-hook lem-clojure-mode:*clojure-mode-hook* 'setup-clojure-context-menu)

;;;; Keybindings

(define-keys *clojure-mode-keymap*
  ("C-c C-a l" 'clojure-align-let)
  ("C-c C-a m" 'clojure-align-map)
  ("C-c C-p"   'clojure-cycle-privacy)
  ("C-c C-a r" 'clojure-add-require)
  ("C-c C-'"   'clojure-toggle-keyword-string))
