(defpackage :lem-vi-mode/registers
  (:use :cl
        :lem)
  (:shadow :yank)
  (:import-from :lem-vi-mode/core
                :vi-current-window)
  (:import-from :lem/common/killring
                :make-item)
  (:import-from :lem/common/ring
                :make-ring
                :ring-ref
                :ring-empty-p
                :ring-push
                :invalid-index-error)
  (:import-from :trivial-types
                :proper-list)
  (:export :register
           :named-register-p
           :numbered-register-p
           :macro-register-p
           :downcase-char
           :yank-region
           :delete-region))
(in-package :lem-vi-mode/registers)

(deftype key-sequence ()
  '(trivial-types:proper-list lem-core::key))

(deftype register () 'character)

(deftype register-designator () '(or character (string 1)))

(defstruct (yank (:constructor make-yank (text &optional (type :char))))
  (text nil :type string)
  (type :char :type (member :char :line)))

(defun append-yank (text1 text2)
  (if (or (eq (yank-type text1) :line)
          (eq (yank-type text2) :line))
      (make-yank
       (with-output-to-string (s)
         (write-string (yank-text text1) s)
         (fresh-line s)
         (write-string (yank-text text2) s)
         (fresh-line s))
       :line)
      (make-yank
       (format nil "~A~A" (yank-text text1) (yank-text text2)))))

(declaim (type hash-table *named-registers*))
(defvar *named-registers* (make-hash-table))

(defvar *yank-text* nil)
(defvar *deletion-history* (make-ring 9))

(defvar *unnamed-register* nil)
(defvar *small-deletion-register* nil)

(defvar *last-ex-command* nil)
(defvar *last-search-query* nil)

(defun downcase-char (char)
  (declare (type character char))
  (cond
    ((char<= #\A char #\Z)
     (code-char
      (+ (char-code char)
         #.(- (char-code #\a) (char-code #\A)))))
    (t char)))

(defun ensure-char (name)
  (check-type name register-designator)
  (if (stringp name)
      (aref name 0)
      name))

(defun named-register-p (name)
  (declare (type character name))
  (or (char<= #\a name #\z)
      (char<= #\A name #\Z)))

(defun numbered-register-p (name)
  (declare (type character name))
  (char<= #\0 name #\9))

(defun macro-register-p (name)
  (or (named-register-p name)
      (numbered-register-p name)
      (char= name #\")))

(defun values-register-item (item)
  (etypecase item
    (null (values nil nil))
    (yank (values (yank-text item)
                  (yank-type item)))
    (key-sequence (values item :key-sequence))))

(defgeneric append-register-item (item1 item2)
  (:method ((item1 t) (item2 t))
    (error "Unmatched type: '~A' and '~A'"
           (type-of item1)
           (type-of item2))))

(defmethod append-register-item ((item1 list) (item2 list))
  (append item1 item2))

(defmethod append-register-item ((item1 yank) (item2 yank))
  (append-yank item1 item2))

(defmethod append-register-item ((item1 null) item2)
  item2)

(defun killring-item-to-yank (item)
  (destructuring-bind (text &optional options)
      item
    (make-yank text (if (member :vi-line options)
                        :line
                        :char))))

(defun yank-to-killring-item (yank)
  (make-item :string (yank-text yank)
             :options (if (eq (yank-type yank) :line)
                          '(:vi-line)
                          nil)))

(defun get-named-register (name)
  (assert (char<= #\a name #\z))
  (gethash name *named-registers*))

(defun set-named-register (name item &key append)
  (assert (char<= #\a name #\z))
  (check-type item (or yank key-sequence))
  (setf (gethash name *named-registers*)
        (if append
            (let ((existing-item (gethash name *named-registers*)))
              (append-register-item existing-item item))
            item)))

(defun get-numbered-register (name)
  (assert (char<= #\0 name #\9))
  (case name
    (#\0 *yank-text*)
    (otherwise
     (if (ring-empty-p *deletion-history*)
         nil
         (let ((n (- (char-code name) #.(char-code #\1))))
           (handler-case (ring-ref *deletion-history* n)
             (invalid-index-error ()
               nil)))))))

(defun set-numbered-register (name item &key append)
  (assert (char<= #\0 name #\9))
  (check-type item (or yank key-sequence))
  (case name
    (#\0
     (setf *yank-text*
           (if append
               (append-register-item *yank-text* item)
               item)))
    (otherwise
     (let ((n (- (char-code name) #.(char-code #\1))))
       (setf (ring-ref *deletion-history* n)
             (if append
                 (let ((existing-item (ring-ref *deletion-history* n)))
                   (append-register-item existing-item item))
                 item)))))
  (values))

(defun yank-region (start end &key type append)
  (with-killring-context (:options (when (eq type :line) :vi-line)
                          :appending (when (eq type :block)
                                       (continue-flag :vi-yank-block)))
    (copy-region start end))
  (let ((item (make-yank (points-to-string start end)
                         (case type
                           (:line :line)
                           (otherwise :char)))))
    (setf *yank-text*
          (if append
              (append-register-item *yank-text* item)
              item)))
  (setf *unnamed-register* #\0)
  (values))

(defun small-deletion-p (start end type)
  (and (= (line-number-at-point start)
          (line-number-at-point end))
       (not (eq type :line))))

(defun delete-region (start end &key type)
  (with-killring-context (:options (when (eq type :line) :vi-line)
                          :appending (when (eq type :block)
                                       (continue-flag :vi-delete-block)))
    (let* ((small (small-deletion-p start end type))
           (string (lem:delete-between-points start end))
           (yank (make-yank string
                            (case type
                              (:line :line)
                              (otherwise :char)))))
      (copy-to-clipboard-with-killring string)
      (unless small
        (ring-push *deletion-history* yank))
      (if small
          (setf *small-deletion-register* yank
                *unnamed-register* #\-)
          (setf *unnamed-register* #\1))))
  (values))

(defun register (name)
  (let ((name (ensure-char name)))
    (declare (type register name))
    (cond
      ((named-register-p name)
       (values-register-item
        (get-named-register (downcase-char name))))
      ((numbered-register-p name)
       (values-register-item
        (get-numbered-register name)))
      (t
       (ecase name
         ;; Unnamed register
         (#\"
          (typecase *unnamed-register*
            (register (register *unnamed-register*))
            (otherwise
             (values-register-item *unnamed-register*))))
         ;; Small delete register
         (#\-
          (values-register-item
           *small-deletion-register*))
         ;; Most recent Ex command (read-only)
         (#\:
          *last-ex-command*)
         ;; Last inserted text (read-only)
         ;(#\.)
         ;; Current file name (read-only)
         (#\%
          (buffer-filename (current-buffer)))
         ;; Alternate file name register
         (#\#
          (buffer-filename (window-buffer (vi-current-window))))
         ;; Expression register
         ;(#\=)
         ;; Selection register
         ;((#\* #\+))
         ;; Blackhole register
         (#\_
          nil)
         ;; Last search register
         (#\/
          *last-search-query*))))))

(defun (setf register) (value name)
  (flet ((value-to-item (value)
           (etypecase value
             (string (make-yank value))
             (key-sequence value))))
    (let ((name (ensure-char name)))
      (declare (type character name))
      (cond
        ((named-register-p name)
         (let ((lower-name (downcase-char name)))
           (set-named-register lower-name
                               (value-to-item value)
                               :append (char<= #\A name #\Z))))
        ((numbered-register-p name)
         (set-numbered-register name
                                (value-to-item value)))
        ((char= name #\")
         (setf *unnamed-register* (value-to-item value)))
        (t
         (check-type value string)
         (ecase name
           (#\-
            (setf *small-deletion-register* (make-yank value)))
           ((#\: #\. #\% #\#)
            (editor-error "Register '\"~A' is read-only." name))
           ;(#\=)
           ;((#\* #\+))
           (#\_ nil)
           (#\/
            (setf *last-search-query* value)))))))
  (values))
