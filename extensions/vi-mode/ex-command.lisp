(defpackage :lem-vi-mode/ex-command
  (:use :cl
        :lem-vi-mode/ex-core)
  (:import-from :lem-vi-mode/jump-motions
                :with-jump-motion)
  (:import-from :lem-vi-mode/options
                :execute-set-command)
  (:import-from :lem-vi-mode/utils
                :change-directory
                :expand-filename-modifiers))
(in-package :lem-vi-mode/ex-command)

(defun ex-write (range filename touch)
  (case (length range)
    (0 (if (string= filename "")
           (lem:save-current-buffer touch)
           (lem:write-file filename)))
    (2 (lem:write-region-file (first range) (second range)
                              (if (string= filename "")
                                  (lem:buffer-filename (lem:current-buffer))
                                  filename)))
    (otherwise (syntax-error))))

(defun ex-write-quit (range filename force touch)
  (ex-write range filename touch)
  (lem-vi-mode/commands:vi-quit force))

(define-ex-command "^e$" (range filename)
  (declare (ignore range))
  (lem:find-file (merge-pathnames (expand-filename-modifiers filename) (uiop:getcwd))))

(define-ex-command "^(w|write)$" (range filename)
  (ex-write range filename t))

(define-ex-command "^update$" (range filename)
  (when (lem:buffer-modified-p (lem:current-buffer))
    (ex-write range filename t)))

(define-ex-command "^wq$" (range filename)
  (ex-write-quit range filename nil t))

(define-ex-command "^wq!$" (range filename)
  (ex-write-quit range filename t t))

(define-ex-command "^q$" (range argument)
  (declare (ignore range argument))
  (lem-vi-mode/commands:vi-quit t))

(define-ex-command "^qa$" (range argument)
  (declare (ignore range argument))
  (lem:exit-lem t))

(define-ex-command "^q!$" (range argument)
  (declare (ignore range argument))
  (lem-vi-mode/commands:vi-quit nil))

(define-ex-command "^qa!$" (range argument)
  (declare (ignore range argument))
  (lem:exit-lem nil))

(define-ex-command "^wqa$" (range filename)
  (ex-write range filename nil)
  (lem:exit-lem t))

(define-ex-command "^wqa!$" (range filename)
  (ex-write range filename nil)
  (lem:exit-lem nil))

(define-ex-command "^(x|xit)$" (range filename)
  (ex-write-quit range filename nil nil))

(define-ex-command "^(x|xit)!$" (range filename)
  (ex-write-quit range filename t nil))

(define-ex-command "^(sp|split)$" (range filename)
  (declare (ignore range))
  (lem:split-active-window-vertically)
  (unless (string= filename "")
    (lem:find-file (merge-pathnames (expand-filename-modifiers filename) (uiop:getcwd)))))

(define-ex-command "^(vs|vsplit)$" (range filename)
  (declare (ignore range))
  (lem:split-active-window-horizontally)
  (unless (string= filename "")
    (lem:find-file (merge-pathnames (expand-filename-modifiers filename) (uiop:getcwd)))))

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
          (lem-vi-mode/ex-parser:parse-subst-argument argument)
        (if (not (lem:with-point ((s start)
                                  (e end))
                   (lem:search-forward-regexp s before e)))
            (lem:message "Pattern not found")
            (lem:with-point ((last-match (lem:with-point ((s start)
                                                          (e end))
                                           (lem:search-backward-regexp e before s))))
              (flet ((rep (start end count)
                       (lem:with-point ((s start)
                                        (e end))
                         (lem/isearch::query-replace-internal before
                                                              after
                                                              #'lem:search-forward-regexp
                                                              #'lem:search-backward-regexp
                                                              :query nil
                                                              :start s
                                                              :end e
                                                              :count count))))
                (if (equal flag "g")
                    (rep start end nil)
                    (progn
                      (lem:move-point (lem:current-point) start)
                      (loop until (lem:point< end (lem:current-point))
                            do (lem:with-point ((replace-start (lem:current-point))
                                                (replace-end (lem:current-point)))
                                 (lem:line-start replace-start)
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
          (subseq lem-vi-mode/ex-core:*command* 1)
          command)))

(define-ex-command "^(buffers|ls|files)$" (range argument)
  (declare (ignore range argument))
  (lem/list-buffers:list-buffers))

(define-ex-command "^(b|buffer)$" (range buffer-name)
  (declare (ignore range))
  (lem:select-buffer buffer-name))

(define-ex-command "^bd(?:elete)?$" (range buffer-name)
  (declare (ignore range))
  (lem:kill-buffer (if (string= buffer-name "")
                       (lem:current-buffer)
                       buffer-name)))

(define-ex-command "^set?$" (range option-string)
  (declare (ignore range))
  (flet ((encode-value (value)
           (typecase value
             (cons (format nil "~{~A~^,~}" value))
             (otherwise value))))
    (multiple-value-bind (option-value option-name old-value isset)
        (execute-set-command option-string)
      (let ((*print-case* :downcase))
        (if (and isset
                 (not (equal option-value old-value)))
            (lem:show-message (format nil "~A: ~S => ~S"
                                      option-name
                                      (encode-value old-value)
                                      (encode-value option-value))
                              :timeout 10)
            (lem:show-message (format nil "~A: ~S" option-name (encode-value option-value))
                              :timeout 10))))))

(define-ex-command "^cd$" (range new-directory)
  (declare (ignore range))
  (let ((new-directory (change-directory (expand-filename-modifiers new-directory))))
    (lem:message "~A" new-directory)))

(define-ex-command "^noh(?:lsearch)?$" (range argument)
  (declare (ignore range argument))
  (lem/isearch:isearch-end))
