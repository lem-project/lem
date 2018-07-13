(defpackage :lem-vi-mode.ex-command
  (:use :cl :lem-vi-mode.ex-core))
(in-package :lem-vi-mode.ex-command)

(defun ex-write (range filename)
  (when (string= filename "")
    (setf filename (lem:buffer-filename (lem:current-buffer))))
  (case (length range)
    (0 (lem:write-file filename))
    (2 (lem:write-region-file (first range) (second range) filename))
    (otherwise (syntax-error))))

(defun ex-write-quit (range filename force)
  (ex-write range filename)
  (lem-vi-mode.commands:vi-quit force))

(define-ex-command ("e") (range filename)
  (declare (ignore range))
  (lem:find-file (merge-pathnames filename
                                  (lem:buffer-directory))))

(define-ex-command ("w" "write") (range filename)
  (ex-write range filename))

(define-ex-command ("wq") (range filename)
  (ex-write-quit range filename nil))

(define-ex-command ("wq!") (range filename)
  (ex-write-quit range filename t))

(define-ex-command ("q") (range argument)
  (declare (ignore range argument))
  (lem-vi-mode.commands:vi-quit t))

(define-ex-command ("qa") (range argument)
  (declare (ignore range argument))
  (lem:exit-lem t))

(define-ex-command ("q!") (range argument)
  (declare (ignore range argument))
  (lem-vi-mode.commands:vi-quit nil))

(define-ex-command ("qa!") (range argument)
  (declare (ignore range argument))
  (lem:exit-lem nil))

(define-ex-command ("sp") (range filename)
  (declare (ignore range))
  (lem:split-active-window-vertically)
  (unless (string= filename "")
    (lem:find-file (lem:expand-file-name filename))))

(define-ex-command ("vs") (range filename)
  (declare (ignore range))
  (lem:split-active-window-horizontally)
  (unless (string= filename "")
    (lem:find-file (lem:expand-file-name filename))))

(define-ex-command ("s" "substitute") (range argument)
  (let (start end)
    (case (length range)
      ((0)
       (setf start (lem:line-start (lem:copy-point *point* :temporary))
             end (lem:line-end (lem:copy-point *point* :temporary))))
      ((2)
       (setf start (first range)
             end (second range))))
    (destructuring-bind (before after flag)
        (lem-vi-mode.ex-parser:parse-subst-argument argument)
      (lem.isearch::query-replace-internal before
                                           after
                                           #'lem:search-forward-regexp
                                           #'lem:search-backward-regexp
                                           :query nil
                                           :start start
                                           :end end
                                           :count (if (equal flag "g")
                                                      nil
                                                      1)))))
