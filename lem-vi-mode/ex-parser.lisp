(defpackage :lem-vi-mode.ex-command
  (:use :cl :lem)
  (:export :search-forward
           :search-backward
           :goto-line
           :current-line
           :last-line
           :all-lines
           :marker
           :offset-line
           :search-forward
           :search-backward
           :goto-current-point
           :range))

(defpackage :lem-vi-mode.ex-parser
  (:use :cl)
  (:export :parse-ex-range))
(in-package :lem-vi-mode.ex-parser)

(defstruct (lexer (:constructor %make-lexer))
  position
  string)

(defun make-lexer (string)
  (%make-lexer :string string :position 0))

(defun lookahead (lexer)
  (char (lexer-string lexer) (lexer-position lexer)))

(defun end-of-string-p (lexer)
  (<= (length (lexer-string lexer))
      (lexer-position lexer)))

(defun lexer-forward (lexer)
  (incf (lexer-position lexer)))

(defun looking-at-p (lexer char)
  (char= char (lookahead lexer)))

(defun accept (lexer char)
  (when (looking-at-p lexer char)
    (lexer-forward lexer)
    t))

(defun skip-whitespace (lexer)
  (loop :while (and (not (end-of-string-p lexer))
                    (char= #\space (lookahead lexer)))
        :do (lexer-forward lexer)))

(defun match-number (lexer)
  (when (digit-char-p (lookahead lexer))
    (parse-integer
     (with-output-to-string (out)
       (loop :until (end-of-string-p lexer)
             :for c := (lookahead lexer)
             :while (digit-char-p c)
             :do (write-char c out)
                 (lexer-forward lexer))))))

(defun match-pattern-1 (lexer separator)
  (when (accept lexer separator)
    (with-output-to-string (out)
      (loop :until (end-of-string-p lexer)
            :for c := (lookahead lexer)
            :until (accept lexer separator)
            :do (lexer-forward lexer)
                (when (char= c #\\)
                  (lexer-forward lexer)
                  (when (end-of-string-p lexer)
                    (return))
                  (setf c (lookahead lexer)))
                (write-char c out)))))

(defun match-pattern (lexer)
  (unless (end-of-string-p lexer)
    (alexandria:when-let ((result (match-pattern-1 lexer #\/)))
      (return-from match-pattern `(lem-vi-mode.ex-command:search-forward ,result)))
    (alexandria:when-let ((result (match-pattern-1 lexer #\?)))
      (return-from match-pattern `(lem-vi-mode.ex-command:search-backward ,result)))))

(defun parse-ex-offset (lexer)
  (skip-whitespace lexer)
  (unless (end-of-string-p lexer)
    (cond ((accept lexer #\+)
           (alexandria:when-let ((number (match-number lexer)))
             `(lem-vi-mode.ex-command:offset-line ,number)))
          ((accept lexer #\-)
           (alexandria:when-let ((number (match-number lexer)))
             `(lem-vi-mode.ex-command:offset-line ,(- number)))))))

(defun parse-ex-line (lexer)
  (skip-whitespace lexer)
  (unless (end-of-string-p lexer)
    (let ((result))
      (cond ((setf result (match-number lexer))
             `(lem-vi-mode.ex-command:goto-line ,result))
            ((setf result (case (lookahead lexer)
                            (#\. '(lem-vi-mode.ex-command:current-line))
                            (#\$ '(lem-vi-mode.ex-command:last-line))
                            (#\% '(lem-vi-mode.ex-command:all-lines))))
             (lexer-forward lexer)
             result)
            ((accept lexer #\')
             (prog1 `(lem-vi-mode.ex-command:marker ,(lookahead lexer))
               (lexer-forward lexer)))
            ((match-pattern lexer))
            ((parse-ex-offset lexer))))))

(defun parse-ex-range-element (lexer)
  (let ((lines (loop
                 :for line := (parse-ex-line lexer)
                 :while line
                 :collect line)))
    (if (null (rest lines))
        (first lines)
        `(progn . ,lines))))

(defun delimiter (lexer)
  (unless (end-of-string-p lexer)
    (cond ((accept lexer #\,)
           (values nil t))
          ((accept lexer #\;)
           (values 'lem-vi-mode.ex-command:goto-current-point t))
          (t
           (values nil nil)))))

(defun parse-ex-range (lexer)
  (let ((range '()))
    (loop :with delim := nil :and contp := nil
          :for elt := (parse-ex-range-element lexer)
          :do (when elt (push elt range))
              (setf (values delim contp) (delimiter lexer))
              (unless contp (return))
              (when delim
                (setf (first range) (list delim (first range)))))
    (when range
      `(lem-vi-mode.ex-command:range . ,(nreverse range)))))

(defun parse-command (lexer)
  (multiple-value-bind (start end)
      (ppcre:scan "^(?:[~&*@<>=:]+|[\\w-]+|!)"
                  (lexer-string lexer)
                  :start (lexer-position lexer))
    (when start
      (setf (lexer-position lexer) end)
      (subseq (lexer-string lexer) start end))))

(defun parse-ex (lexer)
  (let* ((range (parse-ex-range lexer))
         (command (parse-command lexer)))
    (declare (ignore range command))))
