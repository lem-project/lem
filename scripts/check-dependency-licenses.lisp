;;;; Copyright (c) 2026 Yuri Flores Albuquerque. All Rights Reserved.
;;;; SPDX-License-Identifier: LicenseRef-Yeditor-Proprietary
;;;;
;;;; This file is part of Yeditor Core IP (Layers L1-L5).
;;;; Unauthorized copying, modification, or distribution of this file,
;;;; via any medium, is strictly prohibited without a written license agreement.

;;; Loads Lem and every Yeditor system, then classifies the declared license of
;;; each loaded ASDF system. Exit 1 on GPL/AGPL, or on LGPL/LLGPL/MPL/EPL/unknown
;;; licenses not listed in scripts/ip-license-exceptions.txt.
(load ".qlot/setup.lisp")
(require :asdf)

(dolist (asd '("tx/yeditor-tx.asd" "context/yeditor-context.asd"
               "context/yeditor-context-reactivity.asd" "context/yeditor-context-loader.asd"
               "context/yeditor-context-durability.asd" "effects/effects.asd"))
  (asdf:load-asd (merge-pathnames (concatenate 'string "yeditor/" asd) (uiop:getcwd))))
(dolist (s '(:lem :yeditor-tx :yeditor-context :yeditor-context-reactivity
             :yeditor-context-loader :yeditor-context-durability :yeditor-effects))
  (asdf:load-system s))

(defun read-exceptions ()
  (with-open-file (in "scripts/ip-license-exceptions.txt")
    (loop :for line := (read-line in nil) :while line
          :for name := (string-trim " " line)
          :unless (or (zerop (length name)) (char= (char name 0) #\#))
            :collect name)))

(defun word-p (license word)
  "True if WORD occurs in LICENSE not preceded by a letter (so GPL does not match LGPL)."
  (loop :for start := 0 :then (1+ pos)
        :for pos := (search word license :start2 start)
        :while pos
        :thereis (or (zerop pos) (not (alpha-char-p (char license (1- pos)))))))

(defun classify (license)
  (let ((l (string-upcase (or license ""))))
    (flet ((any-word (&rest words) (some (lambda (w) (word-p l w)) words)))
      (cond ((zerop (length (string-trim " " l))) :none)
            ((search "LICENSEREF-YEDITOR-PROPRIETARY" l) :own)
            ((or (any-word "GPL" "AGPL" "GPLV2" "GPLV3") (search "GNU GENERAL" l) (search "AFFERO" l))
             :strong-copyleft)
            ((or (any-word "LGPL" "LLGPL" "MPL" "EPL" "CDDL")
                 (search "LESSER GENERAL" l) (search "MOZILLA" l) (search "ECLIPSE" l))
             :weak-copyleft)
            ((some (lambda (w) (search w l))
                   '("MIT" "BSD" "APACHE" "BOOST" "ZLIB" "ISC" "PUBLIC DOMAIN" "WTFPL"
                     "X11" "EXPAT" "UNLICENSE" "CC0"))
             :permissive)
            (t :unknown)))))

(defun primary (name)
  (let ((slash (position #\/ name))) (if slash (subseq name 0 slash) name)))

(let ((exceptions (read-exceptions))
      (errors nil) (excepted nil) (no-license nil))
  (dolist (name (sort (asdf:already-loaded-systems) #'string<))
    (let* ((license (or (ignore-errors (asdf:system-license (asdf:find-system name)))
                        (ignore-errors (asdf:system-license (asdf:find-system (primary name))))))
           (class (classify license))
           (listed (or (member name exceptions :test #'string=)
                       (member (primary name) exceptions :test #'string=))))
      (case class
        (:strong-copyleft (push (format nil "~A: ~A (GPL/AGPL is never allowed)" name license) errors))
        ((:weak-copyleft :unknown)
         (if listed
             (push (format nil "~A: ~A" name license) excepted)
             (push (format nil "~A: ~A (not in scripts/ip-license-exceptions.txt)" name license) errors)))
        (:none (push name no-license)))))
  (format t "~&Dependency license check~%")
  (format t "Open exceptions (~D):~%~{  ~A~%~}" (length excepted) (reverse excepted))
  (format t "No declared license (~D, review manually): ~{~A~^ ~}~%" (length no-license) (reverse no-license))
  (cond (errors
         (format t "FAIL (~D):~%~{  ~A~%~}" (length errors) (reverse errors))
         (uiop:quit 1))
        (t (format t "OK~%") (uiop:quit 0))))
