(in-package :lem-vi-mode.ex-parser)

(defstruct lexer
  position
  string)

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
      (return-from match-pattern (list :search :forward result)))
    (alexandria:when-let ((result (match-pattern-1 lexer #\?)))
      (return-from match-pattern (list :search :backward result)))))

(defun parse-ex-line (lexer)
  (skip-whitespace lexer)
  (unless (end-of-string-p lexer)
    (let ((result))
      (cond ((setf result (match-number lexer))
             (list :line-number result))
            ((setf result (case (lookahead lexer)
                            (#\. :current-line)
                            (#\$ :last-line)
                            (#\% :all-lines)))
             (lexer-forward lexer)
             (list result))
            ((accept lexer #\')
             (prog1 (list :mark (lookahead lexer))
               (lexer-forward lexer)))
            ((match-pattern lexer))))))

(defun parse-ex-offset (lexer)
  (skip-whitespace lexer)
  (unless (end-of-string-p lexer)
    (cond ((accept lexer #\+)
           (alexandria:when-let ((number (match-number lexer)))
             (list :offset number)))
          ((accept lexer #\-)
           (alexandria:when-let ((number (match-number lexer)))
             (list :offset (- number)))))))

(defun parse-ex-range-element (lexer)
  (let ((list (loop
                :with line := (parse-ex-line lexer)
                :for offset := (parse-ex-offset lexer)
                :collect line
                :when offset :collect it
                :unless (and (eq :search (first line))
                             (setf line (match-pattern lexer)))
                :do (loop-finish))))
    (if (alexandria:length= list 1)
        (first list)
        (cons :patterns list))))

(defun delimiter (lexer)
  (unless (end-of-string-p lexer)
    (cond ((accept lexer #\,)
           (values nil t))
          ((accept lexer #\;)
           (values :goto t))
          (t
           (values nil nil)))))

(defun parse-ex-range (string)
  (let ((lexer (make-lexer :position 0 :string string))
        (range '()))
    (loop :with delim := nil :and contp := nil
          :do (push (parse-ex-range-element lexer) range)
              (setf (values delim contp) (delimiter lexer))
              (unless contp (return))
          (when delim
            (setf (first range) (list :goto (first range)))))
    (nreverse range)))
