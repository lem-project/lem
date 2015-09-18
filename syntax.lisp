(in-package :lem)

(export '(syntax-table
          make-syntax-table
          syntax-add-keyword
          syntax-word-char-p
          syntax-space-char-p
          syntax-symbol-char-p
          syntax-open-paren-char-p
          syntax-closed-paren-char-p
          syntax-pair-open-paren
          syntax-pair-closed-paren
          syntax-parallel-paren
          syntax-string-quote-char-p
          syntax-escape-char-p
          syntax-expr-prefix-char-p
          syntax-skip-expr-prefix-forward
          syntax-skip-expr-prefix-backward
          syntax-line-comment-p
          syntax-start-block-comment-p
          syntax-end-block-comment-p
          syntax-scan-window))

(defvar *syntax-attr-names*
  '(:string-attr
    :comment-attr
    :keyword-attr
    :constant-attr
    :function-name-attr
    :variable-attr))

(defun syntax-init-attributes ()
  (flet ((f (name1 name2)
            (set-attr name1 (get-attr name2))))
    (f :string-attr :green)
    (f :comment-attr :red)
    (f :keyword-attr :blue)
    (f :constant-attr :magenta)
    (f :function-name-attr :cyan)
    (f :variable-attr :yellow)))

(defstruct syntax-table
  (space-chars '(#\space #\tab #\newline))
  (symbol-chars '(#\_))
  (paren-alist '((#\( . #\))
                 (#\[ . #\])
                 (#\{ . #\})))
  (string-quote-chars '(#\"))
  (escape-chars '(#\\))
  expr-prefix-chars
  expr-prefix-forward-function
  expr-prefix-backward-function
  line-comment-preceding-char
  line-comment-following-char
  block-comment-preceding-char
  block-comment-following-char
  keywords)

(defstruct syntax-keyword
  regex-p
  word-p
  test
  test-symbol
  attr
  matched-symbol
  symbol-tov)

(defun syntax-add-keyword (syntax-table
                           test
                           &key
                           regex-p word-p test-symbol attr
                           matched-symbol symbol-tov)
  (push (make-syntax-keyword :test (if regex-p
                                       (ppcre:create-scanner test)
                                       test)
                             :regex-p regex-p
                             :word-p word-p
                             :test-symbol test-symbol
                             :attr attr
                             :matched-symbol matched-symbol
                             :symbol-tov symbol-tov)
        (syntax-table-keywords syntax-table)))

(defun syntax-word-char-p (c)
  (and (characterp c)
       (alphanumericp c)))

(defun syntax-space-char-p (c)
  (member c (syntax-table-space-chars (current-syntax))))

(defun syntax-symbol-char-p (c)
  (or (syntax-word-char-p c)
      (member c (syntax-table-symbol-chars (current-syntax)))))

(defun syntax-open-paren-char-p (c)
  (assoc c (syntax-table-paren-alist (current-syntax))))

(defun syntax-closed-paren-char-p (c)
  (rassoc c (syntax-table-paren-alist (current-syntax))))

(defun syntax-pair-open-paren (c)
  (car (rassoc c (syntax-table-paren-alist (current-syntax)))))

(defun syntax-pair-closed-paren (c)
  (cdr (assoc c (syntax-table-paren-alist (current-syntax)))))

(defun syntax-parallel-paren (c)
  (or (syntax-pair-open-paren c)
      (syntax-pair-closed-paren c)))

(defun syntax-equal-paren-p (x y)
  (flet ((f (c)
            (if (syntax-open-paren-char-p c)
                c
                (syntax-pair-open-paren c))))
    (eql (f x) (f y))))

(defun syntax-string-quote-char-p (c)
  (member c (syntax-table-string-quote-chars (current-syntax))))

(defun syntax-escape-char-p (c)
  (member c (syntax-table-escape-chars (current-syntax))))

(defun syntax-expr-prefix-char-p (c)
  (member c (syntax-table-expr-prefix-chars (current-syntax))))

(defun syntax-skip-expr-prefix-forward ()
  (let ((f (syntax-table-expr-prefix-forward-function (current-syntax))))
    (if f (funcall f) t)))

(defun syntax-skip-expr-prefix-backward ()
  (let ((f (syntax-table-expr-prefix-backward-function (current-syntax))))
    (if f (funcall f) t)))

(defun equal-comment-p (a b x y)
  (and (eql a x)
       (or (null b)
           (eql b y))))

(defun syntax-line-comment-p (c1 c2)
  (equal-comment-p
   (syntax-table-line-comment-preceding-char (current-syntax))
   (syntax-table-line-comment-following-char (current-syntax))
   c1
   c2))

(defun syntax-start-block-comment-p (c1 c2)
  (equal-comment-p
   (syntax-table-block-comment-preceding-char (current-syntax))
   (syntax-table-block-comment-following-char (current-syntax))
   c1
   c2))

(defun syntax-end-block-comment-p (c1 c2)
  (syntax-start-block-comment-p c2 c1))

(defvar *syntax-symbol-tov-list* nil)

(defun syntax-scan-window (window)
  (with-window-range (start-linum end-linum) window
    (syntax-scan-lines window start-linum end-linum)))

(defun syntax-scan-lines (window start-linum end-linum)
  (let* ((buffer (window-buffer window))
         (line (buffer-get-line buffer start-linum))
         (prev (line-prev line))
         (in-string-p (and prev
                           (or (line-start-string-p prev)
                               (line-in-string-p prev))))
         (in-comment-p (and prev
                            (or (line-start-comment-p prev)
                                (line-in-comment-p prev))))
         (*syntax-symbol-tov-list*))
    (do ((line line (line-next line))
         (linum start-linum (1+ linum)))
        ((or (null line)
             (= linum end-linum)))
      (multiple-value-setq (in-string-p in-comment-p)
                           (syntax-scan-line line
                                             in-string-p
                                             in-comment-p)))))

(defun parallel-string-quote (line)
  (do ((line #1=(line-prev line) #1#))
      ((null line))
    (when (line-start-string-p line)
      (let* ((str (line-str line))
             (len (length str))
             (attr (line-get-attribute line (1- len))))
        (do ((pos (- len 2) (1- pos)))
            (nil)
          (when (or (< pos 0)
                    (not (line-contains-attribute line pos attr)))
            (return-from parallel-string-quote
              (schar str (1+ pos)))))))))

(defun syntax-scan-string (line col multiple-lines-p parallel-char)
  (let ((str (line-str line))
        (start-col col))
    (do ((i col (1+ i)))
        ((>= i (length str))
         (line-put-attribute line
                             start-col
                             (length str)
                             (get-attr :string-attr))
         (return (values i nil)))
      (let ((c (schar str i)))
        (cond ((syntax-escape-char-p c)
               (incf i))
              ((and (syntax-string-quote-char-p c)
                    (eql c (if (not multiple-lines-p)
                               parallel-char
                               (parallel-string-quote line))))
               (line-put-attribute line
                                   start-col
                                   (1+ i)
                                   (get-attr :string-attr))
               (return (values i t))))))))

(defun syntax-scan-block-comment (line col)
  (let ((str (line-str line))
        (start-col col))
    (do ((i1 col i2)
         (i2 (1+ col) (1+ i2)))
        ((>= i2 (length str))
         (when (< start-col (length str))
           (line-put-attribute line start-col (length str)
                               (get-attr :comment-attr)))
         (values i2 nil))
      (let ((c1 (schar str i1))
            (c2 (schar str i2)))
        (cond ((syntax-escape-char-p c1)
               (incf i1)
               (incf i2))
              ((syntax-end-block-comment-p c1 c2)
               (line-put-attribute line
                                   start-col
                                   (1+ i2)
                                   (get-attr :comment-attr))
               (return (values i2 t))))))))

(defun syntax-update-symbol-tov ()
  (setq *syntax-symbol-tov-list*
        (loop :for flag-keyword :in *syntax-symbol-tov-list*
          :if (plusp (car flag-keyword))
          :collect (cons (1- (car flag-keyword))
                         (cdr flag-keyword)))))

(defun syntax-matched-word (line sw start end)
  (when (or (not (syntax-keyword-word-p sw))
            (and (or (<= (length (line-str line)) end)
                     (not (syntax-symbol-char-p
                           (aref (line-str line) end))))
                 (or (zerop start)
                     (not (syntax-symbol-char-p
                           (aref (line-str line) (1- start)))))))
    (when (syntax-keyword-matched-symbol sw)
      (push (cons (syntax-keyword-symbol-tov sw)
                  (syntax-keyword-matched-symbol sw))
            *syntax-symbol-tov-list*))
    (when (syntax-keyword-attr sw)
      (line-put-attribute line start end (get-attr (syntax-keyword-attr sw))))
    t))

(defun syntax-scan-word (line start)
  (let ((str (line-str line)))
    (dolist (sw (syntax-table-keywords (current-syntax)))
      (when (or (not (syntax-keyword-test-symbol sw))
                (member (syntax-keyword-test-symbol sw)
                        *syntax-symbol-tov-list*
                        :key #'cdr))
        (cond
         ((syntax-keyword-regex-p sw)
          (multiple-value-bind (start1 end1)
              (ppcre:scan (syntax-keyword-test sw) str :start start)
            (when (and start1 (= start start1)
                       (syntax-matched-word line sw start end1))
              (return-from syntax-scan-word (1- end1)))))
         ((stringp (syntax-keyword-test sw))
          (let ((end (+ start (length (syntax-keyword-test sw)))))
            (when (and (string= str (syntax-keyword-test sw)
                                :start1 start
                                :end1 (when (< end (length str))
                                        end))
                       (syntax-matched-word line sw start end))
              (return-from syntax-scan-word (1- end)))))
         ((functionp (syntax-keyword-test sw))
          (let ((end (or (position-if (complement #'syntax-symbol-char-p)
                                      str :start start)
                         (length str))))
            (when (and (funcall (syntax-keyword-test sw)
                                (subseq str start end))
                       (syntax-matched-word line sw start end))
              (return-from syntax-scan-word end)))))))))

(defun syntax-scan-whitespaces (str i)
  (do ((i i (1+ i)))
      ((or (>= i (length str))
           (not (syntax-space-char-p (schar str i))))
       i)))

(defun syntax-scan-line (line in-string-p in-comment-p)
  (declare (optimize speed))
  (line-clear-attribute line)
  (let ((start-col 0))
    (cond (in-string-p
           (multiple-value-bind (i found-term-p)
               (syntax-scan-string line 0 t nil)
             (cond (found-term-p
                    (setf (line-end-string-p line) t))
                   (t
                    (setf (line-in-string-p line) t)
                    (return-from syntax-scan-line
                      (values t nil))))
             (setq start-col (1+ i))))
          (in-comment-p
           (multiple-value-bind (i found-term-p)
               (syntax-scan-block-comment line 0)
             (cond (found-term-p
                    (setf (line-end-comment-p line) t))
                   (t
                    (setf (line-in-comment-p line) t)
                    (return-from syntax-scan-line
                      (values nil t))))
             (setq start-col (1+ i)))))
    (line-clear-stat line)
    (let ((str (line-str line)))
      (do ((i start-col (1+ i)))
          ((>= i (length str)))
        (syntax-update-symbol-tov)
        (when (<= (length str)
                  (setq i (syntax-scan-whitespaces str i)))
          (return))
        (let ((c (schar str i)))
          (cond ((syntax-escape-char-p c)
                 (incf i))
                ((syntax-string-quote-char-p c)
                 (line-put-attribute line i (1+ i) (get-attr :string-attr))
                 (multiple-value-bind (j found-term-p)
                     (syntax-scan-string line (1+ i) nil c)
                   (setq i j)
                   (unless found-term-p
                     (setf (line-start-string-p line) t)
                     (return (values t nil)))))
                ((syntax-start-block-comment-p c (safe-aref str (1+ i)))
                 (line-put-attribute line i (+ i 2) (get-attr :comment-attr))
                 (multiple-value-bind (j found-term-p)
                     (syntax-scan-block-comment line (+ i 2))
                   (setq i j)
                   (unless found-term-p
                     (setf (line-start-comment-p line) t)
                     (return (values nil t)))))
                ((syntax-line-comment-p c (safe-aref str (1+ i)))
                 (line-put-attribute line
                                     i
                                     (length str)
                                     (get-attr :comment-attr))
                 (return))
                (t
                 (let ((pos (syntax-scan-word line i)))
                   (when pos
                     (setq i pos))))))))))

(defun syntax-scan-buffer (buffer)
  (let ((in-string-p)
        (in-comment-p))
    (map-buffer #'(lambda (line linum)
                    (declare (ignore linum))
                    (multiple-value-setq (in-string-p in-comment-p)
                                         (syntax-scan-line line
                                                           in-string-p
                                                           in-comment-p)))
                buffer)))
