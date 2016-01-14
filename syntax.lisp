;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(syntax-table
          make-syntax-table
          syntax-add-keyword
          syntax-add-keyword-pre
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

(defstruct (syntax-test (:constructor %make-syntax-test))
  thing
  regex-p
  word-p)

(defun make-syntax-test (thing &key regex-p word-p)
  (%make-syntax-test :thing thing :regex-p regex-p :word-p word-p))

(defclass syntax ()
  ((attr
    :initarg :attr
    :initform 0
    :reader syntax-attr
    :type fixnum)))

(defclass syntax-region (syntax)
  ((start
    :initarg :start
    :reader syntax-region-start
    :type syntax-test)
   (end
    :initarg :end
    :reader syntax-region-end
    :type syntax-test)))

(defstruct syntax-match
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
  elements)

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
      (syntax-add-region syntax-table
                         (make-syntax-test (string string-quote-char))
                         (make-syntax-test (string string-quote-char))
                         :string-attr))
    (let ((pre (syntax-table-block-comment-preceding-char syntax-table))
          (flw (syntax-table-block-comment-following-char syntax-table)))
      (when (and pre flw)
        (syntax-add-region syntax-table
                           (make-syntax-test (format nil "~c~c" pre flw))
                           (make-syntax-test (format nil "~c~c" flw pre))
                           :comment-attr)))
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
      matched-symbol (symbol-tov -1))))

(macrolet ((def (name add-f)
                `(defun ,name ,*syntax-add-keyword-lambda-list*
                   (setf (syntax-table-elements syntax-table)
                         (,add-f (syntax-table-elements syntax-table)
                                 (make-syntax-match
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

(defun syntax-add-region (syntax-table start end attr)
  (push (make-instance 'syntax-region :start start :end end :attr attr)
        (syntax-table-elements syntax-table)))

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
           (*syntax-symbol-tov-list* (and prev (line-symbol-tov-list prev))))
      (do ((line line (line-next line))
           (linum start-linum (1+ linum)))
          ((or (null line)
               (= linum end-linum)))
        (syntax-scan-line line)))))

(defun syntax-scan-buffer (buffer)
  (when (and *enable-syntax-highlight*
             (buffer-get buffer :enable-syntax-highlight))
    (map-buffer #'(lambda (line linum)
                    (declare (ignore linum))
                    (syntax-scan-line line))
                buffer)))

(defun syntax-update-symbol-tov ()
  (setq *syntax-symbol-tov-list*
        (loop :for (symbol . tov) :in *syntax-symbol-tov-list*
          :when (/= 0 tov)
          :collect (cons symbol (1- tov)))))

(defun syntax-matched-word (line skw start end)
  (when (or (not (syntax-match-word-p skw))
            (and (or (zerop start)
                     (not (syntax-symbol-char-p
                           (aref (line-str line) (1- start)))))
                 (or (<= (length (line-str line)) end)
                     (not (syntax-symbol-char-p (aref (line-str line) end))))))
    (when (syntax-match-matched-symbol skw)
      (push (cons (syntax-match-matched-symbol skw)
                  (syntax-match-symbol-tov skw))
            *syntax-symbol-tov-list*))
    (when (syntax-match-end-symbol skw)
      (setq *syntax-symbol-tov-list*
            (remove (syntax-match-end-symbol skw)
                    *syntax-symbol-tov-list*
                    :key #'car)))
    (when (syntax-match-attr skw)
      (line-put-attribute line start end (get-attr (syntax-match-attr skw))))
    t))

(defun syntax-position-word-end (str start)
  (or (position-if-not #'syntax-symbol-char-p str
                       :start start)
      (length str)))

(defun syntax-fit-word-p (str start end word-p)
  (or (not word-p)
      (and (or (zerop start)
               (not (syntax-symbol-char-p (aref str (1- start)))))
           (or (<= (length str) end)
               (not (syntax-symbol-char-p (aref str end)))))))

(defun syntax-test-match-p (syntax-test str start)
  (cond
   ((syntax-test-regex-p syntax-test)
    (if (syntax-test-word-p syntax-test)
        (let ((end (syntax-position-word-end str start)))
          (when (ppcre:scan (syntax-test-thing syntax-test)
                            (subseq str start end))
            (return-from syntax-test-match-p (values start end))))
        (multiple-value-bind (start1 end1)
            (ppcre:scan (syntax-test-thing syntax-test) str :start start)
          (when (and start1 (= start start1))
            (return-from syntax-test-match-p (values start1 end1))))))
   ((stringp (syntax-test-thing syntax-test))
    (let ((end (+ start (length (syntax-test-thing syntax-test)))))
      (when (syntax-test-word-p syntax-test)
        (setq end
              (max end
                   (syntax-position-word-end str start))))
      (when (and (string= str (syntax-test-thing syntax-test)
                          :start1 start
                          :end1 (when (< end (length str))
                                  end))
                 (syntax-fit-word-p str start end
                                    (syntax-test-word-p syntax-test)))
        (return-from syntax-test-match-p (values start end)))))))

(defgeneric syntax-scan-token-test (syntax line start))

(defmethod syntax-scan-token-test ((syntax syntax-region) line start)
  )

(defmethod syntax-scan-token-test ((syntax syntax-match) line start)
  (when (or (not (syntax-match-test-symbol syntax))
            (find (syntax-match-test-symbol syntax)
                  *syntax-symbol-tov-list*
                  :key #'car))
    (let ((str (line-str line)))
      (cond
       ((syntax-match-regex-p syntax)
        (cond ((syntax-match-word-p syntax)
               (let ((end (syntax-position-word-end str start)))
                 (when (ppcre:scan (syntax-match-test syntax)
                                   (subseq str start end))
                   (syntax-matched-word line syntax start end)
                   (return-from syntax-scan-token-test (values line (1- end))))))
              (t
               (multiple-value-bind (start1 end1)
                   (ppcre:scan (syntax-match-test syntax)
                               str :start start)
                 (when (and start1
                            (= start start1)
                            (syntax-matched-word line syntax start end1))
                   (return-from syntax-scan-token-test (values line (1- end1))))))))
       ((stringp (syntax-match-test syntax))
        (let ((end (+ start (length (syntax-match-test syntax)))))
          (when (syntax-match-word-p syntax)
            (setq end
                  (max end
                       (syntax-position-word-end str start))))
          (when (and
                 (string= str (syntax-match-test syntax)
                          :start1 start
                          :end1 (when (< end (length str))
                                  end))
                 (syntax-matched-word line syntax start end))
            (return-from syntax-scan-token-test (values line (1- end))))))
       ((functionp (syntax-match-test syntax))
        (let ((end (syntax-position-word-end str start)))
          (when (and (funcall (syntax-match-test syntax) str start end)
                     (syntax-matched-word line syntax start end))
            (return-from syntax-scan-token-test (values line end)))))))))

(defun syntax-scan-token (line start)
  (some #'(lambda (syn)
            (syntax-scan-token-test syn line start))
        (syntax-table-elements (current-syntax))))

(defun syntax-scan-whitespaces (str i)
  (do ((i i (1+ i)))
      ((or (>= i (length str))
           (not (syntax-space-char-p (schar str i))))
       i)))

(defun %syntax-prev-attr (line i)
  (if (= i 0)
      0
      (multiple-value-bind (x y)
          (fat-char (line-fatstr line) (1- i))
        (declare (ignore x))
        y)))

(defun syntax-scan-line (line)
  (line-clear-attribute line)
  (let ((start-col 0)
        (str (line-str line)))
    (do ((i start-col (1+ i)))
        ((>= i (length str)))
      (syntax-update-symbol-tov)
      (when (<= (length str)
                (setq i (syntax-scan-whitespaces str i)))
        (return))
      (let ((c (schar str i)))
        (cond ((syntax-escape-char-p c)
               (line-put-attribute line
                                   i
                                   (min (+ i 2) (line-length line))
                                   (%syntax-prev-attr line i))
               (incf i))
              ((multiple-value-bind (line2 pos)
                   (syntax-scan-token line i)
                 (when (and line2 pos)
                   (setq i pos))))
              (t
               (let ((end (syntax-position-word-end (line-str line) i)))
                 (when (<= i (1- end))
                   (setq i (1- end))))))))
    (setf (line-symbol-tov-list line) *syntax-symbol-tov-list*)))
