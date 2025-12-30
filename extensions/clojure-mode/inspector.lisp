(defpackage :lem-clojure-mode/inspector
  (:use :cl :lem :lem/button)
  (:import-from :lem-clojure-mode/nrepl-client
                :check-nrepl-connection
                :nrepl-eval-sync
                :nrepl-response-value
                :nrepl-response-err)
  (:import-from :lem-clojure-mode
                :clojure-mode
                :clojure-current-namespace)
  (:export :clojure-inspector-mode
           :*clojure-inspector-mode-keymap*
           :clojure-inspect
           :clojure-inspect-last-sexp))

(in-package :lem-clojure-mode/inspector)

;;;; Attributes

(define-attribute inspector-title-attribute
  (t :foreground :base0D :bold t))

(define-attribute inspector-label-attribute
  (t :foreground :base03 :bold t))

(define-attribute inspector-value-attribute
  (t :foreground :base0B))

(define-attribute inspector-type-attribute
  (t :foreground :base0E))

(define-attribute inspector-key-attribute
  (t :foreground :base09))

(define-attribute inspector-index-attribute
  (t :foreground :base08))

;;;; Inspector State

(defvar *inspector-history* nil
  "History of inspected values.")

(defvar *current-value-form* nil
  "The form used to get the current inspected value.")

;;;; Inspector Mode

(define-major-mode clojure-inspector-mode clojure-mode
    (:name "Clojure Inspector"
     :keymap *clojure-inspector-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *clojure-inspector-mode-keymap* "q" 'clojure-inspector-quit)
(define-key *clojure-inspector-mode-keymap* "l" 'clojure-inspector-pop)
(define-key *clojure-inspector-mode-keymap* "Backspace" 'clojure-inspector-pop)
(define-key *clojure-inspector-mode-keymap* "n" 'clojure-inspector-next-section)
(define-key *clojure-inspector-mode-keymap* "p" 'clojure-inspector-prev-section)
(define-key *clojure-inspector-mode-keymap* "Tab" 'clojure-inspector-next-section)
(define-key *clojure-inspector-mode-keymap* "Shift-Tab" 'clojure-inspector-prev-section)
(define-key *clojure-inspector-mode-keymap* "g" 'clojure-inspector-refresh)
(define-key *clojure-inspector-mode-keymap* "Return" 'clojure-inspector-inspect-at-point)

;;;; Buffer Management

(defun inspector-buffer ()
  "Get or create the inspector buffer."
  (or (get-buffer "*clojure-inspector*")
      (let ((buffer (make-buffer "*clojure-inspector*" :enable-undo-p nil)))
        (change-buffer-mode buffer 'clojure-inspector-mode)
        buffer)))

(defun show-inspector ()
  "Show the inspector buffer."
  (let ((buffer (inspector-buffer)))
    (switch-to-window (pop-to-buffer buffer))))

;;;; Value Introspection via nREPL

(defun inspect-form (form &optional ns)
  "Inspect the value of FORM by evaluating it."
  (check-nrepl-connection)
  (let* ((code (format nil "
(let [v ~A]
  {:type (type v)
   :class (class v)
   :value (pr-str v)
   :meta (meta v)
   :count (when (or (coll? v) (string? v)) (count v))
   :keys (when (map? v) (vec (take 100 (keys v))))
   :entries (cond
              (map? v) (vec (take 50 v))
              (set? v) (vec (take 50 v))
              (sequential? v) (vec (take 50 v))
              :else nil)})
" form))
         (responses (nrepl-eval-sync code :ns (or ns "user"))))
    (let ((value (nrepl-response-value responses))
          (err (nrepl-response-err responses)))
      (cond
        (err (editor-error "Inspection error: ~A" err))
        (value (parse-inspect-result value))))))

(defun parse-inspect-result (edn-string)
  "Parse the EDN inspection result."
  ;; Simple EDN parsing for our specific structure
  (let ((result (make-hash-table :test 'equal)))
    (handler-case
        (let ((str (string-trim '(#\space #\newline) edn-string)))
          ;; Extract type
          (alexandria:when-let ((type-match (nth-value 1 (ppcre:scan-to-strings ":type\\s+([^,}]+)" str))))
            (setf (gethash "type" result) (string-trim '(#\space #\,) (aref type-match 0))))
          ;; Extract value
          (alexandria:when-let ((value-match (nth-value 1 (ppcre:scan-to-strings ":value\\s+\"([^\"]*)\"" str))))
            (setf (gethash "value" result) (aref value-match 0)))
          ;; Extract count
          (alexandria:when-let ((count-match (nth-value 1 (ppcre:scan-to-strings ":count\\s+([0-9]+)" str))))
            (setf (gethash "count" result) (parse-integer (aref count-match 0))))
          result)
      (error (e)
        (declare (ignore e))
        (setf (gethash "value" result) edn-string)
        result))))

;;;; Inspector Display

(defun display-inspection (form result)
  "Display the inspection result in the inspector buffer."
  (let ((buffer (inspector-buffer))
        (type (gethash "type" result "unknown"))
        (value (gethash "value" result ""))
        (count (gethash "count" result)))
    (setf *current-value-form* form)
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (with-point ((point (buffer-point buffer)))
        ;; Title
        (insert-string point "Clojure Inspector" :attribute 'inspector-title-attribute)
        (insert-character point #\newline)
        (insert-string point (make-string 40 :initial-element #\-))
        (insert-character point #\newline)
        (insert-character point #\newline)
        ;; Type
        (insert-string point "Type: " :attribute 'inspector-label-attribute)
        (insert-string point type :attribute 'inspector-type-attribute)
        (insert-character point #\newline)
        ;; Count (if applicable)
        (when count
          (insert-string point "Count: " :attribute 'inspector-label-attribute)
          (insert-string point (format nil "~D" count) :attribute 'inspector-value-attribute)
          (insert-character point #\newline))
        ;; Value
        (insert-character point #\newline)
        (insert-string point "Value:" :attribute 'inspector-label-attribute)
        (insert-character point #\newline)
        (insert-string point value :attribute 'inspector-value-attribute)
        (insert-character point #\newline)
        ;; Navigation hint
        (insert-character point #\newline)
        (insert-string point (make-string 40 :initial-element #\-))
        (insert-character point #\newline)
        (insert-string point "Press 'q' to quit, 'l' to go back, 'g' to refresh"
                       :attribute 'inspector-label-attribute)))
    (buffer-start (buffer-point buffer))
    (show-inspector)))

;;;; Commands

(define-command clojure-inspect (form) ((:string "Inspect: "))
  "Inspect a Clojure value."
  (let* ((ns (clojure-current-namespace))
         (result (inspect-form form ns)))
    (when result
      (push (cons form ns) *inspector-history*)
      (display-inspection form result))))

(define-command clojure-inspect-last-sexp () ()
  "Inspect the value of the sexp before point."
  (check-nrepl-connection)
  (with-point ((start (current-point))
               (end (current-point)))
    (when (form-offset start -1)
      (let ((form (points-to-string start end)))
        (clojure-inspect form)))))

(define-command clojure-inspector-quit () ()
  "Quit the inspector."
  (setf *inspector-history* nil)
  (setf *current-value-form* nil)
  (quit-active-window t))

(define-command clojure-inspector-pop () ()
  "Go back to the previous inspected value."
  (if (cdr *inspector-history*)
      (progn
        (pop *inspector-history*)
        (let* ((prev (car *inspector-history*))
               (form (car prev))
               (ns (cdr prev))
               (result (inspect-form form ns)))
          (when result
            (display-inspection form result))))
      (message "No previous value")))

(define-command clojure-inspector-refresh () ()
  "Refresh the current inspection."
  (when *current-value-form*
    (let* ((ns (clojure-current-namespace))
           (result (inspect-form *current-value-form* ns)))
      (when result
        (display-inspection *current-value-form* result)))))

(define-command clojure-inspector-next-section () ()
  "Move to the next section in the inspector."
  (or (search-forward (current-point) ":")
      (buffer-end (current-point))))

(define-command clojure-inspector-prev-section () ()
  "Move to the previous section in the inspector."
  (or (search-backward (current-point) ":")
      (buffer-start (current-point))))

(define-command clojure-inspector-inspect-at-point () ()
  "Inspect the value at point (for nested structures)."
  ;; This is a simplified version - in a full implementation,
  ;; we would track clickable parts of the value
  (message "Use C-c I to inspect a new value"))

;;;; Add keybinding to clojure-mode

(define-key lem-clojure-mode:*clojure-mode-keymap* "C-c I" 'clojure-inspect-last-sexp)
