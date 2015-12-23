;; -*- Mode: LISP; Package: LEM -*-

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
  (set-attr :string-attr (get-attr :green))
  (set-attr :comment-attr (get-attr :red))
  (set-attr :keyword-attr (get-attr :blue))
  (set-attr :constant-attr (get-attr :magenta))
  (set-attr :function-name-attr (get-attr :cyan))
  (set-attr :variable-attr (get-attr :yellow)))

(defstruct syntax-keyword
  regex-p
  word-p
  test
  test-symbol
  end-symbol
  attr
  matched-symbol
  symbol-tov)

(defstruct (syntax-table (:constructor %make-syntax-table))
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

(defun make-syntax-table (&rest args)
  (let ((syntax-table (apply '%make-syntax-table args)))
    (let ((pre (syntax-table-line-comment-preceding-char syntax-table))
          (flw (syntax-table-line-comment-following-char syntax-table)))
      (when pre
        (let ((string (if flw
                          (format nil "~c~c" pre flw)
                          pre)))
          (syntax-add-keyword-pre syntax-table
                                  (format nil "~a.*$" string)
                                  :regex-p t
                                  :attr :comment-attr))))
    (dolist (string-quote-char (syntax-table-string-quote-chars syntax-table))
      (let ((string-symbol (gensym "STRING")))
        (syntax-add-keyword-pre syntax-table
                                (string string-quote-char)
                                :regex-p nil
                                :matched-symbol string-symbol
                                :symbol-tov -1
                                :attr :string-attr)
        (syntax-add-keyword-pre syntax-table
                                "."
                                :regex-p t
                                :test-symbol string-symbol
                                :attr :string-attr)
        (syntax-add-keyword-pre syntax-table
                                (string string-quote-char)
                                :regex-p nil
                                :test-symbol string-symbol
                                :end-symbol string-symbol
                                :attr :string-attr)))
    (let ((pre (syntax-table-block-comment-preceding-char syntax-table))
          (flw (syntax-table-block-comment-following-char syntax-table))
          (comment-symbol (gensym "BLOCK-COMMENT")))
      (when (and pre flw)
        (syntax-add-keyword-pre syntax-table
                                (format nil "~c~c" pre flw)
                                :regex-p nil
                                :matched-symbol comment-symbol
                                :symbol-tov -1
                                :attr :comment-attr)
        (syntax-add-keyword-pre syntax-table
                                "."
                                :regex-p t
                                :test-symbol comment-symbol
                                :attr :comment-attr)
        (syntax-add-keyword-pre syntax-table
                                (format nil "~c~c" flw pre)
                                :regex-p nil
                                :test-symbol comment-symbol
                                :attr :comment-attr)))
    syntax-table))

(defun %syntax-push (list x)
  (cons x list))

(defun %syntax-append (list x)
  (append list (list x)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *syntax-add-keyword-lambda-list*
    '(syntax-table
      test
      &key
      regex-p word-p test-symbol end-symbol attr
      matched-symbol symbol-tov)))

(macrolet ((def (name add-f)
                `(defun ,name ,*syntax-add-keyword-lambda-list*
                   (setf (syntax-table-keywords syntax-table)
                         (,add-f (syntax-table-keywords syntax-table)
                                 (make-syntax-keyword
                                  :test (if regex-p
                                            (ppcre:create-scanner test)
                                            test)
                                  :regex-p regex-p
                                  :word-p word-p
                                  :test-symbol test-symbol
                                  :end-symbol end-symbol
                                  :attr attr
                                  :matched-symbol matched-symbol
                                  :symbol-tov symbol-tov))))))
  (def syntax-add-keyword %syntax-append)
  (def syntax-add-keyword-pre %syntax-push))

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
  (when (and *enable-syntax-highlight*
             (buffer-get (window-buffer window) :enable-syntax-highlight))
    (with-window-range (start-linum end-linum) window
      (syntax-scan-lines window start-linum end-linum))))

(defun syntax-scan-lines (window start-linum end-linum)
  (when (and *enable-syntax-highlight*
             (buffer-get (window-buffer window) :enable-syntax-highlight))
    (let* ((buffer (window-buffer window))
           (line (buffer-get-line buffer start-linum))
           (prev (line-prev line))
           (in-string-p (and prev
                             (or (line-start-string-p prev)
                                 (line-in-string-p prev))))
           (in-comment-p (and prev
                              (or (line-start-comment-p prev)
                                  (line-in-comment-p prev))))
           (*syntax-symbol-tov-list* (and prev
                                          (line-symbol-tov-list prev))))
      (do ((line line (line-next line))
           (linum start-linum (1+ linum)))
          ((or (null line)
               (= linum end-linum)))
        (multiple-value-setq (in-string-p in-comment-p)
                             (syntax-scan-line line
                                               in-string-p
                                               in-comment-p))))))

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
        (loop :for (symbol . tov) :in *syntax-symbol-tov-list*
          :when (/= 0 tov)
          :collect (cons symbol (1- tov)))))

(defun syntax-matched-word (line skw start end)
  (when (or (not (syntax-keyword-word-p skw))
            (and (or (zerop start)
                     (not (syntax-symbol-char-p
                           (aref (line-str line) (1- start)))))
                 (or (<= (length (line-str line)) end)
                     (not (syntax-symbol-char-p (aref (line-str line) end))))))
    (when (syntax-keyword-matched-symbol skw)
      (push (cons (syntax-keyword-matched-symbol skw)
                  (syntax-keyword-symbol-tov skw))
            *syntax-symbol-tov-list*))
    (when (syntax-keyword-end-symbol skw)
      (setq *syntax-symbol-tov-list*
            (remove (syntax-keyword-end-symbol skw)
                    *syntax-symbol-tov-list*
                    :key #'car)))
    (when (syntax-keyword-attr skw)
      (line-put-attribute line start end (get-attr (syntax-keyword-attr skw))))
    t))

(defun syntax-position-word-end (str start)
  (or (position-if-not #'syntax-symbol-char-p str
                       :start start)
      (length str)))

(defun syntax-scan-word (line start)
  (let ((str (line-str line)))
    (dolist (skw (syntax-table-keywords (current-syntax)))
      (when (or (not (syntax-keyword-test-symbol skw))
                (find (syntax-keyword-test-symbol skw)
                      *syntax-symbol-tov-list*
                      :key #'car))
        (cond
         ((syntax-keyword-regex-p skw)
          (cond ((syntax-keyword-word-p skw)
                 (let ((end (syntax-position-word-end str start)))
                   (when (ppcre:scan (syntax-keyword-test skw)
                                     (subseq str start end))
                     (syntax-matched-word line skw start end)
                     (return-from syntax-scan-word (1- end)))))
                (t
                 (multiple-value-bind (start1 end1)
                     (ppcre:scan (syntax-keyword-test skw)
                                 str :start start)
                   (when (and start1
                              (= start start1)
                              (syntax-matched-word line skw start end1))
                     (return-from syntax-scan-word (1- end1)))))))
         ((stringp (syntax-keyword-test skw))
          (let ((end (+ start (length (syntax-keyword-test skw)))))
            (when (syntax-keyword-word-p skw)
              (setq end
                    (max end
                         (syntax-position-word-end str start))))
            (when (and
                   (string= str (syntax-keyword-test skw)
                            :start1 start
                            :end1 (when (< end (length str))
                                    end))
                   (syntax-matched-word line skw start end))
              (return-from syntax-scan-word (1- end)))))
         ((functionp (syntax-keyword-test skw))
          (let ((end (syntax-position-word-end str start)))
            (when (and (funcall (syntax-keyword-test skw) str start end)
                       (syntax-matched-word line skw start end))
              (return-from syntax-scan-word end)))))))
    nil))

(defun syntax-scan-whitespaces (str i)
  (do ((i i (1+ i)))
      ((or (>= i (length str))
           (not (syntax-space-char-p (schar str i))))
       i)))

(defun syntax-scan-line (line in-string-p in-comment-p)
  (line-clear-attribute line)
  (let ((start-col 0))
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
                ((let ((pos (syntax-scan-word line i)))
                   (when pos
                     (setq i pos))))
                (t
                 (let ((end (syntax-position-word-end (line-str line) i)))
                   (when (<= i (1- end))
                     (setq i (1- end))))))))
      (setf (line-symbol-tov-list line) *syntax-symbol-tov-list*))))

(defun syntax-scan-buffer (buffer)
  (when (and *enable-syntax-highlight*
             (buffer-get buffer :enable-syntax-highlight))
    (let ((in-string-p)
          (in-comment-p))
      (map-buffer #'(lambda (line linum)
                      (declare (ignore linum))
                      (multiple-value-setq (in-string-p in-comment-p)
                                           (syntax-scan-line line
                                                             in-string-p
                                                             in-comment-p)))
                  buffer))))
