(defpackage :lem-markdown-mode/syntax-parser
  (:use :cl :lem)
  (:export :make-syntax-parser
           :scan-buffer
           :search-backward-code-block-start
           :search-forward-code-block-end))
(in-package :lem-markdown-mode/syntax-parser)

(defclass syntax-parser () ())

(defun make-syntax-parser ()
  (make-instance 'syntax-parser))

(defmethod lem/buffer/internal::%syntax-scan-region ((parser syntax-parser) start end)
  ;; To simplify the implementation of scanning for syntax consisting of multiple lines,
  ;; always scan the entire buffer.
  (with-point ((start start)
               (end end))
    (buffer-start start)
    (buffer-end end)
    (remove-text-property start end :attribute)
    (scan-region start end)))

(defun put-line-attribute (point attribute)
  (with-point ((start point)
               (end point))
    (line-start start)
    (line-end end)
    (put-text-property start end :attribute attribute)))

(defun start-code-block-line-p (point)
  (ppcre:scan "^```" (line-string point)))

(defun end-code-block-line-p (point)
  (ppcre:scan "^```$" (line-string point)))

(defun scan-code-block (point end)
  (let* ((groups (nth-value 1 (looking-at point "^```(.*)")))
         (language-name (and groups (elt groups 0)))
         (mode (lem-markdown-mode/languages:find-mode-by-language-name language-name))
         (syntax-table (when mode (mode-syntax-table mode))))
    (line-offset point 1)
    (with-point ((start point))
      (loop :while (point< point end)
            :until (end-code-block-line-p point)
            :while (line-offset point 1))
      (cond (syntax-table
             (set-region-major-mode start point mode)
             (syntax-scan-region start
                                 point
                                 :syntax-table syntax-table
                                 :recursive-check nil))
            (t
             (put-text-property start point :attribute 'syntax-string-attribute))))))

(defun scan-region (start end)
  (clear-region-major-mode start end)
  (with-point ((point start))
    (loop :while (point< point end)
          :do (cond ((looking-at point "^#")
                     (put-line-attribute point 'syntax-constant-attribute))
                    ((looking-at point "^>")
                     (put-line-attribute point 'syntax-string-attribute))
                    ((looking-at point "^\\s*[-*+]")
                     (back-to-indentation point)
                     (with-point ((start point)
                                  (end point))
                       (character-offset end 1)
                       (put-text-property start end :attribute 'syntax-keyword-attribute)))
                    ((looking-at point "^\\s*(?:\\d)+\\.\\s")
                     (back-to-indentation point)
                     (with-point ((start point)
                                  (end point))
                       (skip-chars-forward end #'digit-char-p)
                       (character-offset end 1)
                       (put-text-property start end :attribute 'syntax-keyword-attribute)))
                    ((start-code-block-line-p point)
                     (scan-code-block point end)))
          :while (line-offset point 1))))

(defun search-backward-code-block-start (point)
  (with-point ((point point))
    (loop
      :do (when (start-code-block-line-p point)
            (return point))
      :while (line-offset point -1))))

(defun search-forward-code-block-end (point)
  (with-point ((point point))
    (loop
      :do (when (end-code-block-line-p point)
            (return point))
      :while (line-offset point 1))))
