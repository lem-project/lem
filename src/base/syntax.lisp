(in-package :lem-base)

(export '(*global-syntax-highlight*
          enable-syntax-highlight
          enable-syntax-highlight-p
          syntax-table
          fundamental-syntax-table
          make-syntax-table
          make-syntax-test
          syntax-add-match
          syntax-add-region
          syntax-word-char-p
          syntax-space-char-p
          syntax-symbol-char-p
          syntax-open-paren-char-p
          syntax-closed-paren-char-p
          syntax-string-quote-char-p
          syntax-escape-char-p
          syntax-expr-prefix-char-p
          syntax-skip-expr-prefix-forward
          syntax-skip-expr-prefix-backward
          syntax-scan-range
          in-string-p
          in-comment-p
          search-comment-start-forward
          search-comment-start-backward
          search-string-start-forward
          search-string-start-backward
          skip-whitespace-forward
          skip-whitespace-backward
          skip-space-and-comment-forward
          skip-space-and-comment-backward
          symbol-string-at-point
          form-offset
          scan-lists))

(define-editor-variable enable-syntax-highlight nil)
(defvar *global-syntax-highlight* t)

(defstruct (syntax-test (:constructor %make-syntax-test))
  thing
  word-p)

(defun make-syntax-test (thing &key word-p)
  (%make-syntax-test :thing (ppcre:create-scanner thing)
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
  fence-chars
  expr-prefix-chars
  expr-prefix-forward-function
  expr-prefix-backward-function
  line-comment-string
  block-comment-pairs
  region-list
  match-list)

(defun make-syntax-table (&rest args)
  (let ((syntax-table (apply '%make-syntax-table args)))
    (let ((string (syntax-table-line-comment-string syntax-table)))
      (when string
        (syntax-add-region syntax-table
                           (make-syntax-test `(:sequence ,string))
                           (make-syntax-test "$")
                           :attribute *syntax-comment-attribute*)))
    (dolist (string-quote-char (syntax-table-string-quote-chars syntax-table))
      (syntax-add-region syntax-table
                         (make-syntax-test `(:sequence ,(string string-quote-char)))
                         (make-syntax-test `(:sequence ,(string string-quote-char)))
                         :attribute *syntax-string-attribute*))
    (loop :for (start . end) :in (syntax-table-block-comment-pairs syntax-table)
          :do (syntax-add-region syntax-table
                                 (make-syntax-test `(:sequence ,start))
                                 (make-syntax-test `(:sequence ,end))
                                 :attribute *syntax-comment-attribute*))
    syntax-table))

(defun syntax-add-match (syntax-table test
                                      &key test-symbol end-symbol attribute
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
  (push (make-instance 'syntax-region
                       :start start
                       :end end
                       :attribute attribute)
        (syntax-table-region-list syntax-table)))

(defvar *fundamental-syntax-table* (make-syntax-table))

(defun fundamental-syntax-table ()
  *fundamental-syntax-table*)

(defvar *current-syntax* nil)

(defun current-syntax ()
  (or *current-syntax*
      (buffer-syntax-table (current-buffer))))

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

(defun syntax-equal-paren-p (x y)
  (flet ((f (c)
	   (if (syntax-open-paren-char-p c)
	       c
               (car (rassoc c (syntax-table-paren-alist (current-syntax)))))))
    (eql (f x) (f y))))

(defun syntax-string-quote-char-p (c)
  (member c (syntax-table-string-quote-chars (current-syntax))))

(defun syntax-fence-char-p (c)
  (member c (syntax-table-fence-chars (current-syntax))))

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

(flet ((%match (str1 str2 str1-pos)
         (let ((end1 (+ str1-pos (length str2))))
           (when (and (<= end1 (length str1))
                      (string= str1 str2
                               :start1 str1-pos
                               :end1 end1))
             (length str2)))))

  (defun syntax-line-comment-string-p (line-string pos)
    (%match line-string
            (syntax-table-line-comment-string (current-syntax))
            pos))

  (defun syntax-line-comment-p (point)
    (syntax-line-comment-string-p (line-string point)
                                  (point-charpos point)))

  (defun syntax-start-block-comment-p (point)
    (let ((line-string (line-string point))
          (pos (point-charpos point)))
      (dolist (pair (syntax-table-block-comment-pairs (current-syntax)))
        (let ((start (car pair)))
          (let ((result (%match line-string start pos)))
            (when result
              (return (values result pair))))))))

  (defun syntax-end-block-comment-p (point)
    (dolist (pair (syntax-table-block-comment-pairs (current-syntax)))
      (let ((end (cdr pair)))
        (with-point ((point point))
          (character-offset point (- (length end)))
          (let ((result (%match (line-string point)
                                end
                                (point-charpos point))))
            (when result
              (return (values result pair)))))))))


(defun enable-syntax-highlight-p (buffer)
  (and *global-syntax-highlight*
       (value 'enable-syntax-highlight :buffer buffer)))

(defvar *syntax-scan-limit*)
(defvar *syntax-symbol-lifetimes* nil)

(defun syntax-update-symbol-lifetimes ()
  (setq *syntax-symbol-lifetimes*
        (loop :for (symbol . lifetime) :in *syntax-symbol-lifetimes*
	   :when (/= 0 lifetime)
	   :collect (cons symbol (1- lifetime)))))

(defun syntax-test-match-p (syntax-test point &optional optional-key optional-value)
  (let ((string (line-string point)))
    (multiple-value-bind (start end)
        (ppcre:scan (syntax-test-thing syntax-test)
                    string
                    :start (point-charpos point))
      (when (and start
                 (= (point-charpos point) start)
                 (or (not (syntax-test-word-p syntax-test))
                     (<= (length string) end)
                     (not (syntax-symbol-char-p (schar string end)))))
        (if optional-key
            (with-point ((start-point point))
              (line-offset point 0 end)
              (put-text-property start-point point optional-key optional-value))
            (line-offset point 0 end))
        point))))

(defun syntax-scan-region (region point start-charpos)
  (do ((start-charpos start-charpos 0)) (nil)
    (loop
      (cond ((syntax-escape-char-p (character-at point 0))
             (character-offset point 1))
            ((syntax-test-match-p (syntax-region-end region) point 'region-side :end)
             (line-add-property (point-line point)
                                start-charpos (point-charpos point)
                                :attribute (syntax-attribute region)
                                nil)
             (setf (line-%syntax-context (point-line point)) nil)
             (return-from syntax-scan-region (values point t)))
            ((end-line-p point)
             (return)))
      (character-offset point 1))
    (line-add-property (point-line point)
                       start-charpos (line-length (point-line point))
                       :attribute (syntax-attribute region)
                       t)
    (setf (line-%syntax-context (point-line point)) region)
    (unless (line-offset point 1)
      (return-from syntax-scan-region point))
    (when (point<= *syntax-scan-limit* point)
      (return-from syntax-scan-region point))))

(defun syntax-scan-move-action (syntax start)
  (with-point ((end start)
               (cur start))
    (let ((end (funcall (syntax-match-move-action syntax) end)))
      (when (and end (point< cur end))
        (loop :until (same-line-p cur end) :do
              (setf (line-%syntax-context (point-line cur)) syntax)
              (line-offset cur 1))
        (setf (line-%syntax-context (point-line cur)) 'end-move-action)
        (when (syntax-attribute syntax)
          (put-text-property start end :attribute (syntax-attribute syntax)))
        end))))

(defun syntax-scan-token-test (syntax point)
  (etypecase syntax
    (syntax-region
     (let ((start-charpos (point-charpos point))
           (point (syntax-test-match-p (syntax-region-start syntax) point 'region-side :start)))
       (when point
         (syntax-scan-region syntax point start-charpos)
         point)))
    (syntax-match
     (when (or (not (syntax-match-test-symbol syntax))
               (find (syntax-match-test-symbol syntax)
                     *syntax-symbol-lifetimes*
                     :key #'car))
       (with-point ((start point))
         (when (syntax-test-match-p (syntax-match-test syntax) point)
           (when (syntax-match-matched-symbol syntax)
             (push (cons (syntax-match-matched-symbol syntax)
                         (syntax-match-symbol-lifetime syntax))
                   *syntax-symbol-lifetimes*))
           (when (syntax-match-end-symbol syntax)
             (setf *syntax-symbol-lifetimes*
                   (delete (syntax-match-end-symbol syntax)
                           *syntax-symbol-lifetimes*
                           :key #'car)))
           (cond
             ((syntax-match-move-action syntax)
              (let ((goal-point (syntax-scan-move-action syntax start)))
                (when goal-point
                  (move-point point goal-point))))
             ((syntax-attribute syntax)
              (put-text-property start point :attribute (syntax-attribute syntax))
              point)
             (t
              point))))))))

(defun syntax-maybe-scan-region (point)
  (let* ((line (point-line point))
         (prev (line-prev line))
         (syntax (and prev (line-%syntax-context prev))))
    (if (null syntax)
        (setf (line-%syntax-context line) nil)
        (cond
          ((typep syntax 'syntax-region)
           (syntax-scan-region syntax point (point-charpos point)))
          ((typep syntax 'syntax)
           (cond ((eq (line-%syntax-context line) 'end-move-action)
                  (with-point ((cur point))
                    (previous-single-property-change cur :attribute)
                    (let ((goal-point (syntax-scan-move-action syntax cur)))
                      (when goal-point
                        (move-point point goal-point)))))
                 (t
                  (line-add-property (point-line point)
                                     0 (line-length line)
                                     :attribute (syntax-attribute syntax)
                                     t)
                  (line-end point))))
          (t (setf (line-%syntax-context line) nil))))))

(defun syntax-scan-line (point limit)
  (let ((*syntax-scan-limit* limit))
    (syntax-maybe-scan-region point)
    (loop :until (or (end-line-p point)
                     (point<= *syntax-scan-limit* point))
          :do
          (skip-chars-forward point (lambda (c)
                                      (and (syntax-space-char-p c)
                                           (char/= c #\newline))))
          (when (end-line-p point) (return))
          (when (cond ((syntax-escape-char-p (character-at point 0))
                       (unless (character-offset point 2)
                         (buffer-end point)
                         (return))
                       nil)
                      ((dolist (syn (syntax-table-region-list (current-syntax)))
                         (when (syntax-scan-token-test syn point)
                           (return t))))
                      ((dolist (syn (syntax-table-match-list (current-syntax)))
                         (when (syntax-scan-token-test syn point)
                           (return t))))
                      (t
                       (character-offset point 1)
                       (skip-chars-forward point #'syntax-symbol-char-p)
                       t))
            (syntax-update-symbol-lifetimes)))
    (setf (line-%symbol-lifetimes (point-line point))
          *syntax-symbol-lifetimes*)
    (or (line-offset point 1)
        (buffer-end point))))

(defun syntax-scan-range (start end)
  (assert (eq (point-buffer start)
              (point-buffer end)))
  (let ((buffer (point-buffer start)))
    (when (enable-syntax-highlight-p buffer)
      (let ((*current-syntax*
             (buffer-syntax-table buffer))
            (*syntax-symbol-lifetimes*
             (let ((prev (line-prev (point-line start))))
               (and prev (line-%symbol-lifetimes prev)))))
        (with-point ((start start)
                     (end end))
          (line-start start)
          (line-end end)
          (loop
            (line-clear-property (point-line start) :attribute)
            (syntax-scan-line start end)
            (when (point<= end start)
              (return start))))))))


(defmacro with-point-syntax (point &body body)
  `(let ((*current-syntax* (buffer-syntax-table (point-buffer ,point))))
     ,@body))

(defun in-string-p (point)
  (with-point-syntax point
    (and (eq *syntax-string-attribute*
             (text-property-at point :attribute))
         (not (eq :start (text-property-at point 'region-side))))))

(defun in-comment-p (point)
  (with-point-syntax point
    (and (eq *syntax-comment-attribute*
             (text-property-at point :attribute))
         (not (eq :start (text-property-at point 'region-side))))))

(defun %search-syntax-start-forward (point syntax limit)
  (with-point ((curr point))
    (loop
      (unless (next-single-property-change curr :attribute limit)
        (return nil))
      (when (and (eq syntax
                     (text-property-at curr :attribute))
                 (eq :start (text-property-at curr 'region-side)))
        (return (move-point point curr))))))

(defun %search-syntax-start-backward (point syntax limit)
  (with-point ((curr point))
    (loop
      (unless (previous-single-property-change curr :attribute limit)
        (return nil))
      (when (and (eq syntax
                     (text-property-at curr :attribute))
                 (eq :start (text-property-at curr 'region-side)))
        (return (move-point point curr))))))

(defun search-comment-start-forward (point &optional limit)
  (%search-syntax-start-forward point *syntax-comment-attribute* limit))

(defun search-comment-start-backward (point &optional limit)
  (%search-syntax-start-backward point *syntax-comment-attribute* limit))

(defun search-string-start-forward (point &optional limit)
  (%search-syntax-start-forward point *syntax-string-attribute* limit))

(defun search-string-start-backward (point &optional limit)
  (%search-syntax-start-backward point *syntax-string-attribute* limit))


(flet ((f (c)
         (and (not (char= c #\newline))
              (syntax-space-char-p c))))

  (defun skip-whitespace-forward (point &optional (oneline nil))
    (with-point-syntax point
      (if oneline
          (skip-chars-forward point #'f)
          (skip-chars-forward point #'syntax-space-char-p))))

  (defun skip-whitespace-backward (point &optional (oneline nil))
    (with-point-syntax point
      (if oneline
          (skip-chars-backward point #'f)
          (skip-chars-backward point #'syntax-space-char-p)))))


(defun symbol-string-at-point (point)
  (with-point-syntax point
    (with-point ((point point))
      (skip-chars-backward point #'syntax-symbol-char-p)
      (unless (syntax-symbol-char-p (character-at point))
        (return-from symbol-string-at-point nil))
      (with-point ((start point))
        (skip-chars-forward point #'syntax-symbol-char-p)
        (points-to-string start point)))))

(let ((cache (make-hash-table :test 'equal)))
  (defun %create-pair-regex (pair)
    (let ((tree
           `(:positive-lookahead
             (:alternation
              (:sequence ,(car pair))
              (:sequence ,(cdr pair))))))
      (or (gethash tree cache)
          (setf (gethash tree cache)
                (ppcre:create-scanner tree))))))

(defun %skip-comment-forward (point)
  (multiple-value-bind (n pair)
      (syntax-start-block-comment-p point)
    (cond (n
           (let ((regex (%create-pair-regex pair)))
             (with-point ((curr point))
               (character-offset curr n)
               (loop :with depth := 1
                     :do (cond ((not (search-forward-regexp curr regex))
                                (return (values nil nil)))
                               ((match-string-at curr (cdr pair))
                                (character-offset curr (length (cdr pair)))
                                (when (= 0 (decf depth))
                                  (return (values (move-point point curr) t))))
                               (t
                                (character-offset curr (length (car pair)))
                                (incf depth)))))))
          ((syntax-line-comment-p point)
           (values (line-offset point 1) t))
          (t
           (values nil t)))))

(defun %skip-comment-backward (point)
  (multiple-value-bind (n pair)
      (syntax-end-block-comment-p point)
    (if n
        (let ((regex (%create-pair-regex pair)))
          (with-point ((curr point))
            (character-offset curr (- n))
            (loop :with depth := 1
                  :do (cond ((not (search-backward-regexp curr regex))
                             (return (values nil nil)))
                            ((match-string-at curr (car pair))
                             (when (= 0 (decf depth))
                               (return (values (move-point point curr) t))))
                            (t
                             (incf depth))))))
        (let ((line-comment-pos (%position-line-comment (line-string point)
                                                        (point-charpos point)
                                                        nil)))
          (if line-comment-pos
              (values (line-offset point 0 line-comment-pos) t)
              (values nil t))))))

(defun %position-line-comment (string end in-token)
  (loop :for i := 0 :then (1+ i)
        :while (< i end)
        :do (let ((c (schar string i)))
              (ecase in-token
                ((:string)
                 (cond ((syntax-escape-char-p c)
                        (incf i))
                       ((syntax-string-quote-char-p c)
                        (setf in-token nil))))
                ((:fence)
                 (cond ((syntax-escape-char-p c)
                        (incf i))
                       ((syntax-fence-char-p c)
                        (setf in-token nil))))
                ((nil)
                 (cond ((syntax-line-comment-string-p string i)
                        (return i))
                       ((syntax-escape-char-p c)
                        (incf i))
                       ((syntax-string-quote-char-p c)
                        (setf in-token :string))
                       ((syntax-fence-char-p c)
                        (setf in-token :fence))))))
        :finally (ecase in-token
                   ((:string)
                    (return (%position-line-comment string end :string)))
                   ((:fence)
                    (return (%position-line-comment string end :fence)))
                   ((nil)
                    (return nil)))))

(defun skip-space-and-comment-forward (point)
  (with-point-syntax point
    (loop
      (skip-chars-forward point #'syntax-space-char-p)
      (multiple-value-bind (result success)
          (%skip-comment-forward point)
        (unless result
          (return success))))))

(defun skip-space-and-comment-backward (point)
  (with-point-syntax point
    (if (%position-line-comment (line-string point) (point-charpos point) nil)
        (skip-chars-backward point #'syntax-space-char-p)
        (loop
          (skip-chars-backward point #'syntax-space-char-p)
          (multiple-value-bind (result success)
              (%skip-comment-backward point)
            (unless result
              (return success)))))))

(defun %sexp-escape-p (point offset)
  (let ((count 0))
    (loop :with string := (line-string point)
          :for i :downfrom (+ (1- (point-charpos point)) offset) :to 0
          :do (if (syntax-escape-char-p (schar string i))
                  (incf count)
                  (return)))
    (values (oddp count) count)))

(defun %sexp-symbol-p (c)
  (or (syntax-symbol-char-p c)
      (syntax-escape-char-p c)
      (syntax-expr-prefix-char-p c)))

(defun %skip-symbol-forward (point)
  (loop :for c := (character-at point 0)
        :do
        (cond ((syntax-escape-char-p c)
               (unless (character-offset point 2)
                 (return)))
              ((syntax-fence-char-p c)
               (unless (%skip-fence-forward point)
                 (return)))
              ((not (or (syntax-symbol-char-p c)
                        (syntax-expr-prefix-char-p c)))
               (return point))
              (t
               (unless (character-offset point 1)
                 (return))))))

(defun %skip-symbol-backward (point)
  (loop :for c := (character-at point -1)
        :do (multiple-value-bind (escape-p skip-count)
                (%sexp-escape-p point -1)
              (cond (escape-p
                     (character-offset point (- (1+ skip-count))))
                    ((syntax-fence-char-p c)
                     (unless (%skip-fence-backward point)
                       (return)))
                    ((or (syntax-symbol-char-p c)
                         (syntax-expr-prefix-char-p c)
                         (syntax-escape-char-p c))
                     (character-offset point -1))
                    (t
                     (return point))))))

(defun %skip-string-forward (point)
  (loop :with quote-char := (character-at point 0) :do
     (unless (character-offset point 1)
       (return nil))
     (let ((c (character-at point)))
       (cond ((syntax-escape-char-p c)
	      (character-offset point 1))
	     ((and (syntax-string-quote-char-p c)
		   (char= c quote-char))
	      (character-offset point 1)
	      (return point))))))

(defun %skip-string-backward (point)
  (character-offset point -1)
  (loop :with quote-char := (character-at point) :do
     (unless (character-offset point -1)
       (return nil))
     (if (%sexp-escape-p point 0)
	 (character-offset point -1)
	 (let ((c (character-at point)))
	   (cond ((and (syntax-string-quote-char-p c)
		       (char= c quote-char))
		  (return point)))))))

(defun %skip-fence-forward (point)
  (loop :with fence-char := (character-at point 0)
        :do
        (unless (character-offset point 1)
          (return nil))
        (let ((c (character-at point)))
          (cond ((syntax-escape-char-p c)
                 (character-offset point 1))
                ((and (syntax-fence-char-p c)
                      (char= c fence-char))
                 (character-offset point 1)
                 (return point))))))

(defun %skip-fence-backward (point)
  (character-offset point -1)
  (loop :with fence-char := (character-at point)
        :do
        (unless (character-offset point -1)
          (return nil))
        (if (%sexp-escape-p point 0)
            (character-offset point -1)
            (let ((c (character-at point)))
              (cond ((and (syntax-fence-char-p c)
                          (char= c fence-char))
                     (return point)))))))

(defun %skip-list-forward (point depth)
  (loop :with paren-stack := '() :do
     (unless (skip-space-and-comment-forward point)
       (return nil))
     (when (end-buffer-p point)
       (return nil))
     (let ((c (character-at point 0)))
       (cond ((syntax-open-paren-char-p c)
	      (push c paren-stack)
	      (character-offset point 1)
	      (when (zerop (incf depth))
		(return point)))
	     ((syntax-closed-paren-char-p c)
	      (unless (or (and (< 0 depth)
			       (null paren-stack))
			  (syntax-equal-paren-p c (car paren-stack)))
		(return nil))
	      (pop paren-stack)
	      (character-offset point 1)
	      (when (zerop (decf depth))
		(return point)))
	     ((syntax-string-quote-char-p c)
	      (%skip-string-forward point))
             ((syntax-fence-char-p c)
              (%skip-fence-forward point))
	     ((syntax-escape-char-p c)
              (unless (character-offset point 2)
                (return nil)))
	     (t
	      (character-offset point 1))))))

(defun %skip-list-backward (point depth)
  (loop :with paren-stack := '() :do
     (unless (skip-space-and-comment-backward point)
       (return nil))
     (when (start-buffer-p point)
       (return nil))
     (let ((c (character-at point -1)))
       (cond ((%sexp-escape-p point -1)
	      (character-offset point -1))
	     ((syntax-closed-paren-char-p c)
	      (push c paren-stack)
	      (character-offset point -1)
	      (when (zerop (incf depth))
		(return point)))
	     ((syntax-open-paren-char-p c)
	      (unless (or (and (< 0 depth)
			       (null paren-stack))
			  (syntax-equal-paren-p c (car paren-stack)))
		(return nil))
	      (pop paren-stack)
	      (character-offset point -1)
	      (when (zerop (decf depth))
		(return point)))
	     ((syntax-string-quote-char-p c)
	      (%skip-string-backward point))
             ((syntax-fence-char-p c)
              (%skip-fence-backward point))
	     (t
	      (character-offset point -1))))))

(defun %form-offset-positive (point)
  (skip-space-and-comment-forward point)
  (when (end-buffer-p point)
    (return-from %form-offset-positive nil))
  (syntax-skip-expr-prefix-forward point)
  (skip-chars-forward point #'syntax-expr-prefix-char-p)
  (unless (end-buffer-p point)
    (let ((c (character-at point)))
      (cond ((or (syntax-symbol-char-p c)
                 (syntax-escape-char-p c))
             (%skip-symbol-forward point))
            ((syntax-expr-prefix-char-p c)
             (character-offset point 1))
            ((syntax-open-paren-char-p c)
             (%skip-list-forward point 0))
            ((syntax-closed-paren-char-p c)
             nil)
            ((syntax-string-quote-char-p c)
             (%skip-string-forward point))
            ((syntax-fence-char-p c)
             (%skip-fence-forward point))
            (t
             (character-offset point 1))))))

(defun %form-offset-negative (point)
  (skip-space-and-comment-backward point)
  (when (start-buffer-p point)
    (return-from %form-offset-negative nil))
  (let ((c (character-at point -1)))
    (prog1 (cond ((or (syntax-symbol-char-p c)
                      (syntax-escape-char-p c)
                      (syntax-expr-prefix-char-p c))
                  (%skip-symbol-backward point))
                 ((syntax-closed-paren-char-p c)
                  (%skip-list-backward point 0))
                 ((syntax-open-paren-char-p c)
                  nil)
                 ((syntax-string-quote-char-p c)
                  (%skip-string-backward point))
                 ((syntax-fence-char-p c)
                  (%skip-fence-backward point))
                 (t
                  (character-offset point -1)))
      (skip-chars-backward point #'syntax-expr-prefix-char-p)
      (syntax-skip-expr-prefix-backward point))))

(defun form-offset (point n)
  (with-point-syntax point
    (with-point ((curr point))
      (when (cond ((plusp n)
                   (dotimes (_ n t)
                     (unless (%form-offset-positive curr)
                       (return nil))))
                  (t
                   (dotimes (_ (- n) t)
                     (unless (%form-offset-negative curr)
                       (return nil)))))
        (move-point point curr)))))

(defun scan-lists (point n depth &optional no-errors)
  (with-point-syntax point
    (with-point ((curr point))
      (when (cond ((plusp n)
                   (dotimes (_ n t)
                     (unless (%skip-list-forward curr depth)
                       (if no-errors
                           (return nil)
                           (scan-error)))))
                  (t
                   (dotimes (_ (- n) t)
                     (unless (%skip-list-backward curr depth)
                       (if no-errors
                           (return nil)
                           (scan-error))))))
        (move-point point curr)))))


(defstruct parser-state
  type
  token-start-point
  end-char
  block-comment-depth
  block-comment-pair
  paren-stack)

(defun parse-partial-sexp (from to &optional (state (make-parser-state)))
  (assert (eq (point-buffer from)
              (point-buffer to)))
  (with-point-syntax from
    (with-point ((p from))
      (let ((type (parser-state-type state))
            (token-start-point (parser-state-token-start-point state))
            (end-char (parser-state-end-char state))
            (block-comment-depth (parser-state-block-comment-depth state))
            (block-comment-pair (parser-state-block-comment-pair state))
            (paren-stack (parser-state-paren-stack state)))
        (block outer
          (loop
            (case type
              (:string
               #1=(loop
                    (when (point<= to p)
                      (return-from outer))
                    (let ((c (character-at p)))
                      (cond ((syntax-escape-char-p c)
                             (character-offset p 1))
                            ((char= c end-char)
                             (setf end-char nil)
                             (setf type nil)
                             (setf token-start-point nil)
                             (return (character-offset p 1))))
                      (character-offset p 1))))
              (:fence
               #1#)
              (:block-comment
               (let ((regex (%create-pair-regex block-comment-pair)))
                 (loop
                   (unless (search-forward-regexp p regex to)
                     (move-point p to)
                     (return-from outer))
                   (cond
                     ((match-string-at p (cdr block-comment-pair))
                      (character-offset p (length (car block-comment-pair)))
                      (when (= 0 (decf block-comment-depth))
                        (setf block-comment-depth nil)
                        (setf block-comment-pair nil)
                        (setf type nil)
                        (setf token-start-point nil)
                        (return p)))
                     (t
                      (character-offset p (length (cdr block-comment-pair)))
                      (incf block-comment-depth))))))
              (:line-comment
               (when (and (point<= p to)
                          (same-line-p p to))
                 (return-from outer))
               (line-offset p 1)
               (setf type nil)
               (setf token-start-point nil))
              (otherwise
               (loop
                 (when (point<= to p)
                   (return-from outer))
                 (let ((c (character-at p)))
                   (cond
                     ((syntax-escape-char-p c)
                      (character-offset p 1))
                     ((or (syntax-string-quote-char-p c)
                          (syntax-fence-char-p c))
                      (setf type (if (syntax-string-quote-char-p c)
                                     :string
                                     :fence))
                      (setf end-char c)
                      (setf token-start-point (copy-point p :temporary))
                      (character-offset p 1)
                      (return))
                     ((multiple-value-bind (n pair)
                          (syntax-start-block-comment-p p)
                        (when n
                          (setf token-start-point (copy-point p :temporary))
                          (setf type :block-comment)
                          (setf block-comment-depth 1)
                          (setf block-comment-pair pair)
                          (character-offset p n)
                          (return))))
                     ((syntax-line-comment-p p)
                      (setf type :line-comment)
                      (setf token-start-point (copy-point p :temporary))
                      (return))
                     ((syntax-open-paren-char-p c)
                      (push c paren-stack))
                     ((syntax-closed-paren-char-p c)
                      (when (syntax-equal-paren-p c (car paren-stack))
                        (pop paren-stack))))
                   (character-offset p 1)))))))
        (without-interrupts
          (setf (parser-state-type state) type
                (parser-state-token-start-point state) token-start-point
                (parser-state-end-char state) end-char
                (parser-state-block-comment-depth state) block-comment-depth
                (parser-state-block-comment-pair state) block-comment-pair
                (parser-state-paren-stack state) paren-stack))
        state))))
