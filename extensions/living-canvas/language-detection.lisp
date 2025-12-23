(defpackage #:lem-living-canvas/language
  (:use #:cl)
  (:export #:detect-language
           #:language-for-buffer
           #:language-for-file
           #:*mode-name-language-map*
           #:*extension-language-map*))
(in-package #:lem-living-canvas/language)

;;; Language Detection
;;;
;;; This module provides language detection for Living Canvas.
;;; It uses a hierarchical approach: major mode → file extension → content.

(defparameter *mode-name-language-map*
  '(("python-mode" . :python)
    ("js-mode" . :javascript)
    ("typescript-mode" . :typescript)
    ("lisp-mode" . :common-lisp)
    ("go-mode" . :go))
  "Association list mapping major mode names (strings) to language keywords.

Uses mode names as strings to avoid package dependency issues at compile time.
This is the primary detection method when a buffer is available,
as the major mode is the most reliable indicator of language.")

(defparameter *extension-language-map*
  '(;; Python
    ("py" . :python)
    ("pyw" . :python)
    ;; JavaScript
    ("js" . :javascript)
    ("mjs" . :javascript)
    ("cjs" . :javascript)
    ("jsx" . :javascript)
    ;; TypeScript
    ("ts" . :typescript)
    ("tsx" . :typescript)
    ("mts" . :typescript)
    ("cts" . :typescript)
    ;; Common Lisp
    ("lisp" . :common-lisp)
    ("cl" . :common-lisp)
    ("asd" . :common-lisp)
    ("lsp" . :common-lisp)
    ;; Go
    ("go" . :go))
  "Association list mapping file extensions to language keywords.

Used as fallback when major mode is not available or for file paths
without an associated buffer.")

(defun language-for-buffer (buffer)
  "Detect language for BUFFER based on its major mode.

Arguments:
  BUFFER - A Lem buffer object

Returns:
  Language keyword (e.g., :python) or NIL if mode is not recognized"
  (when buffer
    (let* ((mode (lem:buffer-major-mode buffer))
           (mode-name (when mode (string-downcase (symbol-name mode)))))
      (cdr (assoc mode-name *mode-name-language-map* :test #'string=)))))

(defun language-for-file (pathname)
  "Detect language for PATHNAME based on its file extension.

Arguments:
  PATHNAME - A pathname designator (pathname, string, or nil)

Returns:
  Language keyword (e.g., :python) or NIL if extension is not recognized"
  (when pathname
    (let ((ext (pathname-type (pathname pathname))))
      (when ext
        (cdr (assoc ext *extension-language-map* :test #'string-equal))))))

(defun detect-language (source)
  "Detect programming language for SOURCE.

SOURCE can be:
  - A Lem buffer: Uses major mode first, then filename extension
  - A pathname: Uses file extension
  - A string: Returns NIL (cannot detect from raw code without heuristics)

Returns:
  Language keyword (e.g., :python, :javascript, :common-lisp) or NIL

The detection hierarchy is:
  1. Major mode (most reliable when buffer is available)
  2. File extension (reliable fallback)
  3. Content inspection (not implemented, returns NIL)"
  (typecase source
    (lem:buffer
     (or (language-for-buffer source)
         (let ((filename (lem:buffer-filename source)))
           (when filename
             (language-for-file filename)))))
    (pathname
     (language-for-file source))
    (string
     ;; Cannot reliably detect language from raw code without complex heuristics
     nil)
    (t nil)))
