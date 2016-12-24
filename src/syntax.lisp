(in-package :lem)

(export '(*enable-syntax-highlight*
          syntax-table
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
          syntax-scan-buffer

          skip-whitespace-forward
          skip-whitespace-backward
          skip-space-and-comment-forward
          skip-space-and-comment-backward
          symbol-string-at-point))

(defvar *enable-syntax-highlight* t)

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
  ((attribute
    :initarg :attribute
    :initform 0
    :reader syntax-attribute
    :type (or null attribute))))

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
    :reader syntax-match-symbol-lifetime)
   (move-action
    :initarg :move-action
    :initform nil
    :reader syntax-match-move-action)))

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
                            :attribute *syntax-comment-attribute*))))
    (dolist (string-quote-char (syntax-table-string-quote-chars syntax-table))
      (syntax-add-region syntax-table
                         (make-syntax-test (string string-quote-char))
                         (make-syntax-test (string string-quote-char))
                         :attribute *syntax-string-attribute*))
    (let ((pre (syntax-table-block-comment-preceding-char syntax-table))
          (flw (syntax-table-block-comment-following-char syntax-table)))
      (when (and pre flw)
        (syntax-add-region syntax-table
                           (make-syntax-test (format nil "~c~c" pre flw))
                           (make-syntax-test (format nil "~c~c" flw pre))
                           :attribute *syntax-comment-attribute*)))
    syntax-table))

(defun syntax-add-match (syntax-table test &key test-symbol end-symbol attribute
                                      matched-symbol (symbol-lifetime -1) move-action)
  (push (make-instance 'syntax-match
                       :test test
                       :test-symbol test-symbol
                       :end-symbol end-symbol
                       :attribute attribute
                       :matched-symbol matched-symbol
                       :symbol-lifetime symbol-lifetime
                       :move-action move-action)
        (syntax-table-match-list syntax-table))
  t)

(defun syntax-add-region (syntax-table start end &key attribute)
  (push (make-instance 'syntax-region :start start :end end :attribute attribute)
        (syntax-table-region-list syntax-table)))

(defvar *current-syntax* nil)

(defun current-syntax ()
  (or *current-syntax*
      (mode-syntax-table (buffer-major-mode))))

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

(defun syntax-skip-expr-prefix-forward (point)
  (let ((f (syntax-table-expr-prefix-forward-function (current-syntax))))
    (if f (funcall f point) t)))

(defun syntax-skip-expr-prefix-backward (point)
  (let ((f (syntax-table-expr-prefix-backward-function (current-syntax))))
    (if f (funcall f point) t)))

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

(defun enable-syntax-highlight-p (buffer)
  (and *enable-syntax-highlight*
       (get-bvar :enable-syntax-highlight :buffer buffer)))

(defvar *syntax-scan-window-recursive-p* nil)

(defun syntax-scan-window (window)
  (check-type window window)
  (when (and (enable-syntax-highlight-p (window-buffer window))
             (null *syntax-scan-window-recursive-p*))
    (let ((*syntax-scan-window-recursive-p* t))
      (window-see window)
      (syntax-scan-lines (window-view-marker window)
                         (or (line-offset (copy-marker (window-view-marker window) :temporary)
                                          (window-height window))
                             (buffers-end (window-buffer window)))))))

(defun syntax-scan-buffer (buffer)
  (check-type buffer buffer)
  (when (enable-syntax-highlight-p buffer)
    (syntax-scan-lines (buffers-start buffer) (buffers-end buffer))))

(defun syntax-scan-current-view ()
  (cond
    ((get-bvar 'already-visited)
     (syntax-scan-window (current-window)))
    (t
     (setf (get-bvar 'already-visited) t)
     (syntax-scan-buffer (current-buffer)))))

(defun syntax-scan-lines (start end)
  (assert (eq (marker-buffer start)
              (marker-buffer end)))
  (let ((buffer (marker-buffer start)))
    (when (enable-syntax-highlight-p buffer)
      (let* ((line (get-line/marker start))
             (prev (line-prev line))
             (*syntax-symbol-lifetimes* (and prev (line-%symbol-lifetimes prev)))
             (*current-syntax* (mode-syntax-table (buffer-major-mode buffer))))
        (save-excursion
          (setf (current-buffer) buffer)
          (move-point (current-marker) start)
          (loop :until (or (null line)
                           (marker<= end (current-marker)))
                :do
                (setf line (%syntax-scan-line line))
                (unless (line-offset (current-marker) 1)
                  (return))
                (setf line (line-next line))))))))

(defun syntax-update-symbol-lifetimes ()
  (setq *syntax-symbol-lifetimes*
        (loop :for (symbol . lifetime) :in *syntax-symbol-lifetimes*
          :when (/= 0 lifetime)
          :collect (cons symbol (1- lifetime)))))

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
               (line-add-property line start end :attribute (syntax-attribute syntax) nil)
               (return-from syntax-scan-token-test end))
              (t
               (line-add-property line
                                  start
                                  (line-length line)
                                  :attribute
                                  (syntax-attribute syntax)
                                  t)
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
          (when (syntax-match-matched-symbol syntax)
            (push (cons (syntax-match-matched-symbol syntax)
                        (syntax-match-symbol-lifetime syntax))
                  *syntax-symbol-lifetimes*))
          (when (syntax-match-end-symbol syntax)
            (setq *syntax-symbol-lifetimes*
                  (remove (syntax-match-end-symbol syntax)
                          *syntax-symbol-lifetimes*
                          :key #'car)))
          (cond
            ((syntax-match-move-action syntax)
             (line-offset (current-marker) 0 start1)
             (let ((start (copy-marker (current-marker) :temporary))
                   (end (funcall (syntax-match-move-action syntax)
                                 (copy-marker (current-marker) :temporary))))
               (cond ((and end (marker< start end))
                      (with-marker ((cur start))
                        (loop :until (same-line-p cur end) :do
                              (line-clear-property line :attribute)
                              (setf (line-%region line) syntax)
                              (setf line (line-next line))
                              (line-offset cur 1)))
                      (setf (line-%region line) nil)
                      (put-text-property start end :attribute (syntax-attribute syntax))
                      (move-point (current-marker) end)
                      (cons (marker-charpos end) line)))))
            (t
             (line-add-property line start1 end1 :attribute (syntax-attribute syntax) nil)
             (1- end1))))))))

(defun syntax-scan-token (line start)
  (flet ((f (syn) (syntax-scan-token-test syn line start)))
    (or (some #'f (syntax-table-region-list (current-syntax)))
        (some #'f (syntax-table-match-list (current-syntax))))))

(defun syntax-scan-whitespaces (str i)
  (do ((i i (1+ i)))
      ((or (>= i (length str))
           (not (syntax-space-char-p (schar str i))))
       i)))

(defun syntax-continue-region-p (line)
  (let ((prev (line-prev line)))
    (and prev (line-%region prev))))

(defun syntax-scan-line-region (line region)
  (when region
    (typecase region
      (syntax-region
       (let ((end (syntax-search-region-end region (line-str line) 0)))
         (cond (end
                (setf (line-%region line) nil)
                (line-add-property line 0 end :attribute (syntax-attribute region) nil)
                end)
               (t
                (setf (line-%region line) region)
                (line-add-property line 0
                                   (line-length line)
                                   :attribute
                                   (syntax-attribute region)
                                   t)
                (length (line-str line))))))
      (otherwise
       (line-add-property line 0
                          (line-length line)
                          :attribute
                          (syntax-attribute region)
                          t)
       (length (line-str line))))))

(defun %syntax-scan-line (line)
  (line-clear-property line :attribute)
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
               (incf i))
              ((let ((result (syntax-scan-token line i)))
                 (cond
                   ((null result) nil)
                   ((consp result)
                    (destructuring-bind (pos . new-line) result
                      (setf line new-line)
                      (setf str (line-str line))
                      (setf i pos)))
                   (t
                    (setf i result)))))
              (t
               (let ((end (syntax-position-word-end (line-str line) i)))
                 (when (<= i (1- end))
                   (setq i (1- end))))))))
    (setf (line-%symbol-lifetimes line) *syntax-symbol-lifetimes*)
    line))

(defun skip-whitespace-forward (point)
  (skip-chars-forward point #'syntax-space-char-p))

(defun skip-whitespace-backward (point)
  (skip-chars-backward point #'syntax-space-char-p))

(defun skip-space-and-comment-forward (point)
  (loop
    (skip-whitespace-forward point)
    (unless (and (not (eq *syntax-comment-attribute* (text-property-at point :attribute -1)))
                 (eq *syntax-comment-attribute* (text-property-at point :attribute)))
      (return t))
    (unless (next-single-property-change point :attribute)
      (return nil))))

(defun skip-space-and-comment-backward (point)
  (loop
    (skip-whitespace-backward point)
    (unless (and (not (eq *syntax-comment-attribute* (text-property-at point :attribute)))
                 (eq *syntax-comment-attribute* (text-property-at point :attribute -1)))
      (return t))
    (unless (previous-single-property-change point :attribute)
      (return nil))))

(defun symbol-string-at-point (point)
  (with-marker ((point point))
    (skip-chars-backward point #'syntax-symbol-char-p)
    (unless (syntax-symbol-char-p (character-at point))
      (return-from symbol-string-at-point nil))
    (with-marker ((start point))
      (skip-chars-forward point #'syntax-symbol-char-p)
      (points-to-string start point))))
