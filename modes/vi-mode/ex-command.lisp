(defpackage :lem-vi-mode.ex-command
  (:use :cl :lem-vi-mode.ex-core)
  (:import-from #:lem-vi-mode.jump-motions
                #:with-jump-motion))
(in-package :lem-vi-mode.ex-command)

(defun ex-write (range filename)
  (case (length range)
    (0 (if (string= filename "")
           (lem:save-buffer)
           (lem:write-file filename)))
    (2 (lem:write-region-file (first range) (second range)
                              (if (string= filename "")
                                  (lem:buffer-filename (lem:current-buffer))
                                  filename)))
    (otherwise (syntax-error))))

(defun ex-write-quit (range filename force)
  (ex-write range filename)
  (lem-vi-mode.commands:vi-quit force))

(define-ex-command "^e$" (range filename)
  (declare (ignore range))
  (lem:find-file (merge-pathnames filename
                                  (lem:buffer-directory))))

(define-ex-command "^(w|write)$" (range filename)
  (ex-write range filename))

(define-ex-command "^update$" (range filename)
  (when (lem:buffer-modified-p (lem:current-buffer))
    (ex-write range filename)))

(define-ex-command "^wq$" (range filename)
  (ex-write-quit range filename nil))

(define-ex-command "^wq!$" (range filename)
  (ex-write-quit range filename t))

(define-ex-command "^q$" (range argument)
  (declare (ignore range argument))
  (lem-vi-mode.commands:vi-quit t))

(define-ex-command "^qa$" (range argument)
  (declare (ignore range argument))
  (lem:exit-lem t))

(define-ex-command "^q!$" (range argument)
  (declare (ignore range argument))
  (lem-vi-mode.commands:vi-quit nil))

(define-ex-command "^qa!$" (range argument)
  (declare (ignore range argument))
  (lem:exit-lem nil))

(define-ex-command "^wqa$" (range filename)
  (ex-write range filename)
  (lem:exit-lem t))

(define-ex-command "^wqa!$" (range filename)
  (ex-write range filename)
  (lem:exit-lem nil))

(define-ex-command "^(sp|split)$" (range filename)
  (declare (ignore range))
  (lem:split-active-window-vertically)
  (unless (string= filename "")
    (lem:find-file (merge-pathnames filename
                                    (lem:buffer-directory)))))

(define-ex-command "^(vs|vsplit)$" (range filename)
  (declare (ignore range))
  (lem:split-active-window-horizontally)
  (unless (string= filename "")
    (lem:find-file (merge-pathnames filename
                                    (lem:buffer-directory)))))

(define-ex-command "^(s|substitute)$" (range argument)
  (with-jump-motion
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
        (if (not (lem:search-forward-regexp start before end))
            (lem:message "Pattern not found")
            (lem:with-point ((last-match (lem:search-backward-regexp end before start)))
              (flet ((rep (start end count)
                       (lem.isearch::query-replace-internal before
                                                            after
                                                            #'lem:search-forward-regexp
                                                            #'lem:search-backward-regexp
                                                            :query nil
                                                            :start start
                                                            :end end
                                                            :count count)))
                (if (equal flag "g")
                    (rep start end nil)
                    (progn
                      (lem:move-point (lem:current-point) start)
                      (loop until (lem:point< end (lem:current-point))
                            do (lem:with-point ((replace-start (lem:current-point))
                                                (replace-end (lem:current-point)))
                                 (lem:line-end replace-end)
                                 (rep replace-start replace-end 1))
                               (lem:next-logical-line 1)
                               (lem:line-start (lem:current-point))))))
               (let ((p (lem:current-point)))
                 (lem:move-point p last-match)
                 (lem:line-start p))))))))

(define-ex-command "^!" (range command)
  (declare (ignore range))
  (lem:pipe-command
   (format nil "~A ~A"
          (subseq lem-vi-mode.ex-core:*command* 1)
          command)))

(define-ex-command "^(buffers|ls|files)$" (range argument)
  (declare (ignore range argument))
  (lem.list-buffers:list-buffers))
