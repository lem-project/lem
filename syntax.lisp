;; -*- Mode: LISP; Package: LEM -*-

(in-package :lem)

(export '(syntax-table
          make-syntax-table
          make-syntax-test
          syntax-add-match
          syntax-add-region
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
          syntax-scan-window
          syntax-after-tag
          syntax-before-tag
          syntax-following-tag
          syntax-preceding-tag
          syntax-forward-search-tag-end
          syntax-backward-search-tag-start))

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
  (%make-syntax-test :thing (if regex-p
                                (ppcre:create-scanner thing)
                                thing)
                     :regex-p regex-p
                     :word-p word-p))

(defclass syntax ()
  ((attr
    :initarg :attr
    :initform 0
    :reader syntax-attr
    :type symbol)
   (tag
    :initarg :tag
    :initform nil
    :reader syntax-tag
    :type symbol)))

(defclass syntax-region (syntax)
  ((start
    :initarg :start
    :reader syntax-region-start
    :type syntax-test)
   (end
    :initarg :end
    :reader syntax-region-end
    :type syntax-test)))

(defclass syntax-match (syntax)
  ((test
    :initarg :test
    :initform nil
    :reader syntax-match-test)
   (test-symbol
    :initarg :test-symbol
    :initform nil
    :reader syntax-match-test-symbol)
   (end-symbol
    :initarg :end-symbol
    :initform nil
    :reader syntax-match-end-symbol)
   (matched-symbol
    :initarg :matched-symbol
    :initform nil
    :reader syntax-match-matched-symbol)
   (symbol-lifetime
    :initarg :symbol-lifetime
    :initform nil
    :reader syntax-match-symbol-lifetime)))

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
  region-list
  match-list)

(defun make-syntax-table (&rest args)
  (let ((syntax-table (apply '%make-syntax-table args)))
    (let ((pre (syntax-table-line-comment-preceding-char syntax-table))
          (flw (syntax-table-line-comment-following-char syntax-table)))
      (when pre
        (let ((string (if flw
                          (format nil "~c~c" pre flw)
                          pre)))
          (syntax-add-match syntax-table
                            (make-syntax-test (format nil "~a.*$" string)
                                              :regex-p t)
                            :attr :comment-attr))))
    (dolist (string-quote-char (syntax-table-string-quote-chars syntax-table))
      (syntax-add-region syntax-table
                         (make-syntax-test (string string-quote-char))
                         (make-syntax-test (string string-quote-char))
                         :attr :string-attr))
    (let ((pre (syntax-table-block-comment-preceding-char syntax-table))
          (flw (syntax-table-block-comment-following-char syntax-table)))
      (when (and pre flw)
        (syntax-add-region syntax-table
                           (make-syntax-test (format nil "~c~c" pre flw))
                           (make-syntax-test (format nil "~c~c" flw pre))
                           :attr :comment-attr)))
    syntax-table))

(defun syntax-add-match (syntax-table test &key test-symbol end-symbol attr
                                      matched-symbol (symbol-lifetime -1) tag)
  (push (make-instance 'syntax-match
                       :test test
                       :test-symbol test-symbol
                       :end-symbol end-symbol
                       :attr attr
                       :matched-symbol matched-symbol
                       :symbol-lifetime symbol-lifetime
                       :tag tag)
        (syntax-table-match-list syntax-table))
  t)

(defun syntax-add-region (syntax-table start end &key attr tag)
  (push (make-instance 'syntax-region :start start :end end :attr attr :tag tag)
        (syntax-table-region-list syntax-table)))

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

(defvar *syntax-symbol-lifetimes* nil)

(defun syntax-scan-window (window)
  (when (and *enable-syntax-highlight*
             (get-bvar :enable-syntax-highlight :buffer (window-buffer window)))
    (with-window-range (start-linum end-linum) window
      (syntax-scan-lines window start-linum end-linum))))

(defun syntax-scan-lines (window start-linum end-linum)
  (when (and *enable-syntax-highlight*
             (get-bvar :enable-syntax-highlight :buffer (window-buffer window)))
    (let* ((buffer (window-buffer window))
           (line (buffer-get-line buffer start-linum))
           (prev (line-prev line))
           (*syntax-symbol-lifetimes* (and prev (line-%symbol-lifetimes prev))))
      (do ((line line (line-next line))
           (linum start-linum (1+ linum)))
          ((or (null line)
               (= linum end-linum)))
        (syntax-scan-line line)))))

(defun syntax-scan-buffer (buffer)
  (when (and *enable-syntax-highlight*
             (get-bvar :enable-syntax-highlight :buffer buffer))
    (map-buffer #'(lambda (line linum)
                    (declare (ignore linum))
                    (syntax-scan-line line))
                buffer)))

(defun syntax-update-symbol-lifetimes ()
  (setq *syntax-symbol-lifetimes*
        (loop :for (symbol . lifetime) :in *syntax-symbol-lifetimes*
          :when (/= 0 lifetime)
          :collect (cons symbol (1- lifetime)))))

(defun syntax-matched-word (line syntax start end)
  (when (syntax-match-matched-symbol syntax)
    (push (cons (syntax-match-matched-symbol syntax)
                (syntax-match-symbol-lifetime syntax))
          *syntax-symbol-lifetimes*))
  (when (syntax-match-end-symbol syntax)
    (setq *syntax-symbol-lifetimes*
          (remove (syntax-match-end-symbol syntax)
                  *syntax-symbol-lifetimes*
                  :key #'car)))
  (when (syntax-attr syntax)
    (line-put-attribute line start end (get-attr (syntax-attr syntax))))
  (line-add-tag line start end (syntax-tag syntax))
  t)

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

(defun syntax-search-region-end (region str start)
  (do ((i start (1+ i)))
      ((>= i (length str)))
    (if (syntax-escape-char-p (aref str i))
        (incf i)
        (multiple-value-bind (_start end)
            (syntax-test-match-p (syntax-region-end region) str i)
          (declare (ignore _start))
          (when end
            (return end))))))

(defgeneric syntax-scan-token-test (syntax line start))

(defmethod syntax-scan-token-test ((syntax syntax-region) line start)
  (multiple-value-bind (_start end)
      (syntax-test-match-p (syntax-region-start syntax)
                           (line-str line)
                           start)
    (declare (ignore _start))
    (when end
      (let ((end (syntax-search-region-end syntax (line-str line) end)))
        (cond (end
               (line-put-attribute line start end
                                   (get-attr (syntax-attr syntax)))
               (line-add-tag line start end (syntax-tag syntax))
               (return-from syntax-scan-token-test (1- end)))
              (t
               (line-put-attribute line start (length (line-str line))
                                   (get-attr (syntax-attr syntax)))
               (line-add-tag line start (length (line-str line))
                             (syntax-tag syntax))
               (setf (line-%region line) syntax)
               (return-from syntax-scan-token-test
                 (length (line-str line)))))))))

(defmethod syntax-scan-token-test ((syntax syntax-match) line start)
  (when (or (not (syntax-match-test-symbol syntax))
            (find (syntax-match-test-symbol syntax)
                  *syntax-symbol-lifetimes*
                  :key #'car))
    (let ((str (line-str line)))
      (multiple-value-bind (start1 end1)
          (syntax-test-match-p (syntax-match-test syntax)
                               str start)
        (when (and start1 end1)
          (syntax-matched-word line syntax start1 end1)
          (return-from syntax-scan-token-test (1- end1)))))))

(defun syntax-scan-token (line start)
  (flet ((f (syn) (syntax-scan-token-test syn line start)))
    (or (some #'f (syntax-table-region-list (current-syntax)))
        (some #'f (syntax-table-match-list (current-syntax))))))

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

(defun syntax-continue-region-p (line)
  (let ((prev (line-prev line)))
    (and prev (line-%region prev))))

(defun syntax-scan-line-region (line region)
  (when region
    (let ((end (syntax-search-region-end region (line-str line) 0)))
      (cond (end
             (setf (line-%region line) nil)
             (line-put-attribute line 0 end (get-attr (syntax-attr region)))
             (line-add-tag line 0 end (syntax-tag region))
             end)
            (t
             (setf (line-%region line) region)
             (line-put-attribute line 0 (length (line-str line))
                                 (get-attr (syntax-attr region)))
             (line-add-tag line 0 (length (line-str line)) (syntax-tag region))
             (length (line-str line)))))))

(defun syntax-scan-line (line)
  (line-clear-attribute line)
  (line-clear-tags line)
  (let* ((region (syntax-continue-region-p line))
         (start-pos (or (syntax-scan-line-region line region) 0))
         (str (line-str line)))
    (unless region
      (setf (line-%region line) nil))
    (do ((i start-pos (1+ i)))
        ((>= i (length str)))
      (syntax-update-symbol-lifetimes)
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
              ((let ((pos (syntax-scan-token line i)))
                 (when pos (setq i pos))))
              (t
               (let ((end (syntax-position-word-end (line-str line) i)))
                 (when (<= i (1- end))
                   (setq i (1- end))))))))
    (setf (line-%symbol-lifetimes line) *syntax-symbol-lifetimes*)))

(defun %syntax-pos-tag (pos)
  (let ((line (buffer-get-line (current-buffer) (current-linum))))
    (when (= 0 pos)
      (unless (setq line (line-prev line))
        (return-from %syntax-pos-tag nil))
      (setq pos (length (line-str line))))
    (loop :for (start end tag) :in (line-%tags line) :do
      (when (<= start pos (1- end))
        (return tag)))))

(defun syntax-after-tag (&optional (n 1))
  (save-excursion
   (shift-position n)
   (%syntax-pos-tag (current-charpos))))

(defun syntax-before-tag (&optional (n 1))
  (save-excursion
   (shift-position (- (1- n)))
   (%syntax-pos-tag (current-charpos))))

(defun syntax-following-tag ()
  (%syntax-pos-tag (1+ (current-charpos))))

(defun syntax-preceding-tag ()
  (%syntax-pos-tag (current-charpos)))

(defun syntax-forward-search-tag-end (tag0)
  (do ((line (buffer-get-line (current-buffer) (current-linum))
             (line-next line))
       (linum (current-linum) (1+ linum))
       (charpos (current-charpos) 0)
       (straddle-p))
      ((null line))
    (let ((found-tag-p nil))
      (loop :with tags := (sort (copy-list (line-%tags line)) #'< :key #'car)
        :for (start end tag) :in tags
        :do (when (eq tag0 tag)
              (cond ((= start charpos)
                     (when (/= end (line-length line))
                       (point-set (make-point linum end))
                       (return-from syntax-forward-search-tag-end t))
                     (setq straddle-p t)
                     (setq found-tag-p t)
                     (return))
                    (t
                     (return-from syntax-forward-search-tag-end nil)))))
      (when (and (not found-tag-p) straddle-p)
        (point-set (make-point (1- linum) (line-length (line-prev line))))
        (return t)))))

(defun syntax-backward-search-tag-start (tag0)
  (do* ((line (buffer-get-line (current-buffer) (current-linum))
              (line-prev line))
        (linum (current-linum) (1- linum))
        (charpos (current-charpos)
                (and (line-p line) (line-length line)))
        (straddle-p))
      ((null line))
    (let ((found-tag-p nil))
      (loop :with tags := (line-%tags line)
        :for (start end tag) :in tags
        :do (when (eq tag0 tag)
              (cond ((= charpos end)
                     (when (/= start 0)
                       (point-set (make-point linum start))
                       (return-from syntax-backward-search-tag-start t))
                     (setq straddle-p t)
                     (setq found-tag-p t)
                     (return))
                    (t
                     (return-from syntax-backward-search-tag-start nil)))))
      (when (and (not found-tag-p) straddle-p)
        (point-set (make-point (1+ linum) 0))
        (return t)))))
