(defpackage :lem-vi-mode/tests/utils
  (:use :cl
        :lem)
  (:shadow :with-current-buffer)
  (:import-from :rove
                :form-description
                :diag
                :testing)
  (:import-from :lem-base
                :*current-buffer*)
  (:import-from :lem-fake-interface
                :with-fake-interface)
  (:import-from :lem-vi-mode
                :vi-mode)
  (:import-from :lem-vi-mode/core
                :change-state
                :normal
                :insert
                :current-state)
  (:import-from :lem-vi-mode/visual
                :visual-line
                :visual-block)
  (:import-from :cl-ppcre
                :scan
                :regex-replace)
  (:import-from :alexandria
                :remove-from-plistf
                :with-gensyms)
  (:export :with-test-buffer
           :with-vi-tests
           :cmd
           :pos=
           :text=
           :buf=))
(in-package :lem-vi-mode/tests/utils)

(defun change-state-by-keyword (state-keyword)
  (change-state
   (ecase state-keyword
     (:normal 'normal)
     (:insert 'insert)
     (:visual 'visual-char)
     (:visual-line 'visual-line)
     (:visual-block 'visual-block))))

;; TODO: Support visual mode
(defun parse-buffer-string (buffer-string)
  (multiple-value-bind (start end)
      (ppcre:scan "(?<!\\\\)\\[" buffer-string)
    (if start
        (let ((len (length buffer-string)))
          (assert
           (or (and (< end len)
                    (char= (aref buffer-string end) #\]))
               (and (< (1+ end) len)
                    (char= (aref buffer-string (1+ end)) #\]))))
          (values (ppcre:regex-replace "\\[([^\\]])?\\]"
                                       buffer-string
                                       "\\1")
                  end))
        (values buffer-string 1))))

;; TODO: Support visual mode
(defun %make-buffer-string (buffer-text buffer-pos)
  (let ((state (current-state)))
    (apply #'concatenate 'string
           (subseq buffer-text 0 (1- buffer-pos))
           (ecase state
             (normal
               (list
                 (format nil "[~C]" (aref buffer-text (1- buffer-pos)))
                 (subseq buffer-text buffer-pos)))
             (insert
               (list
                 "[]"
                 (subseq buffer-text (1- buffer-pos))))))))

(defun make-buffer-string (buffer)
  (%make-buffer-string (buffer-text buffer)
                       (position-at-point (buffer-point buffer))))

;; TODO: Support more escape sequences
(defun text-backslashed (text)
  (ppcre:regex-replace-all "\\n" text "\\n"))

(defmacro with-test-buffer ((var buffer-string
                             &rest buffer-args
                             &key name (temporary t temporary-specified-p)
                             &allow-other-keys)
                            &body body)
  (declare (ignore temporary))
  (unless temporary-specified-p
    (setf (getf buffer-args :temporary) t))
  (remove-from-plistf buffer-args :name)
  (with-gensyms (point buffer-content position)
    `(let* ((,var (make-buffer ,name ,@buffer-args))
            (,point (buffer-point ,var)))
       (multiple-value-bind (,buffer-content ,position)
           (parse-buffer-string ,buffer-string)
         (insert-string ,point ,buffer-content)
         (move-to-position ,point ,position))
       (setf (current-buffer) ,var)
       ,@body)))

(defmacro with-current-buffer ((buffer) &body body)
  `(let ((lem-base::*current-buffer* ,buffer))
     ,@body))

(defmacro with-vi-tests ((buffer &key (state :normal)) &body body)
  `(with-fake-interface ()
     (with-current-buffer (,buffer)
       (lem-vi-mode:vi-mode)
       (change-state-by-keyword ,state)
       (rove:testing (format nil "[buf] \"~A\""
                             (text-backslashed
                               (make-buffer-string (current-buffer))))
         (labels ((cmd (keys)
                    (check-type keys string)
                    (rove:diag (format nil "[cmd] ~A" keys))
                    (execute-key-sequence
                      (loop for c across keys
                            collect (make-key :sym (string c)))))
                  (pos= (expected-point)
                    (point= (current-point) expected-point))
                  (text= (expected-buffer-text)
                    (string= (buffer-text (current-buffer))
                             expected-buffer-text))
                  (buf= (expected-buffer-string)
                    (check-type expected-buffer-string string)
                    (multiple-value-bind (expected-buffer-text expected-position)
                        (parse-buffer-string expected-buffer-string)
                      (with-point ((p (current-point)))
                        (move-to-position p expected-position)
                        (and (text= expected-buffer-text)
                             (pos= p))))))
           ,@body)))))

(defun point-coord (point)
  (values (line-number-at-point point)
          (point-charpos point)))

(defmethod form-description ((function (eql 'lem:point=)) args values &key negative)
  (multiple-value-bind (expected-line expected-col)
      (point-coord (second values))
    (multiple-value-bind (actual-line actual-col)
        (point-coord (first values))
      (format nil "Expect ~W~:[~; not~] to be at (~D, ~D), but at (~D, ~D)"
              (first args)
              negative
              expected-line expected-col
              actual-line actual-col))))

(defmethod form-description ((function (eql 'pos=)) args values &key negative)
  (form-description 'point=
                    (cons '(current-point) args)
                    (cons (current-point) values)
                    :negative negative))

(defmethod form-description ((function (eql 'text=)) args values &key negative)
  (let ((expected-text (first values))
        (actual-text (buffer-text (current-buffer))))
    (format nil "Expect ~W~:[~; not~] to be ~S (actual: ~S)"
            (first args)
            negative
            (text-backslashed expected-text)
            (text-backslashed actual-text))))

(defmethod form-description ((function (eql 'buf=)) args values &key negative)
  (declare (ignore args))
  (format nil "Expect the buffer~:[~; not~] to be \"~A\" (actual: \"~A\")"
          negative
          (text-backslashed (first values))
          (text-backslashed
           (make-buffer-string (current-buffer)))))
