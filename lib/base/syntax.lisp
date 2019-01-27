(in-package :lem-base)

(export '(syntax-table
          set-syntax-parser
          fundamental-syntax-table
          current-syntax
          with-current-syntax
          make-syntax-table
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
          skip-space-and-comment-forward
          skip-space-and-comment-backward
          form-offset
          scan-lists
          skip-whitespace-forward
          skip-whitespace-backward
          skip-symbol-forward
          skip-symbol-backward
          symbol-string-at-point
          make-pps-state
          pps-state-state-type
          pps-state-token-start-point
          pps-state-end-char
          pps-state-block-comment-depth
          pps-state-block-pair
          pps-state-paren-stack
          pps-state-paren-depth
          parse-partial-sexp
          syntax-ppss
          pps-state-string-p
          pps-state-comment-p
          pps-state-string-or-comment-p
          in-string-p
          in-comment-p
          in-string-or-comment-p
          maybe-beginning-of-string
          maybe-beginning-of-comment
          maybe-beginning-of-string-or-comment))

(defstruct syntax-table
  (space-chars '(#\space #\tab #\newline))
  (symbol-chars '(#\_))
  (paren-pairs '((#\( . #\))
                 (#\[ . #\])
                 (#\{ . #\})))
  (string-quote-chars '(#\"))
  (escape-chars '(#\\))
  fence-chars
  expr-prefix-chars
  expr-suffix-chars
  expr-prefix-forward-function
  expr-prefix-backward-function
  line-comment-string
  block-comment-pairs
  block-string-pairs
  parser)

(defun set-syntax-parser (syntax-table parser)
  (setf (syntax-table-parser syntax-table) parser))

(defvar *fundamental-syntax-table* (make-syntax-table))

(defun fundamental-syntax-table ()
  *fundamental-syntax-table*)

(defvar *current-syntax* nil)

(defun current-syntax ()
  (or *current-syntax*
      (buffer-syntax-table (current-buffer))
      *fundamental-syntax-table*))

(defmacro with-current-syntax (syntax &body body)
  `(let ((*current-syntax* ,syntax))
     ,@body))

(defmacro with-point-syntax (point &body body)
  `(with-current-syntax (buffer-syntax-table (point-buffer ,point))
     ,@body))

(defun syntax-word-char-p (c)
  (and (characterp c)
       (alphanumericp c)))

(defun syntax-space-char-p (c)
  (member c (syntax-table-space-chars (current-syntax))))

(defun syntax-symbol-char-p (c)
  (or (syntax-word-char-p c)
      (member c (syntax-table-symbol-chars (current-syntax)))))

(defun syntax-open-paren-char-p (c)
  (assoc c (syntax-table-paren-pairs (current-syntax))))

(defun syntax-closed-paren-char-p (c)
  (rassoc c (syntax-table-paren-pairs (current-syntax))))

(defun syntax-equal-paren-p (x y)
  (flet ((f (c)
           (if (syntax-open-paren-char-p c)
               c
               (car (rassoc c (syntax-table-paren-pairs (current-syntax)))))))
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

  (defun syntax-start-block-syntax-string-p (line-string pos pairs)
    (dolist (pair pairs)
      (let ((start (car pair)))
        (let ((result (%match line-string start pos)))
          (when result
            (return (values result pair)))))))

  (defun syntax-end-block-syntax-string-p (line-string pos pairs)
    (dolist (pair pairs)
      (let* ((end (cdr pair))
             (pos (- pos (length end))))
        (when (<= 0 pos)
          (let ((result (%match line-string end pos)))
            (when result
              (return (values result pair))))))))

  (defun syntax-line-comment-p (point)
    (%match (line-string point)
            (syntax-table-line-comment-string (current-syntax))
            (point-charpos point)))

  (defun syntax-start-block-comment-p (point)
    (syntax-start-block-syntax-string-p (line-string point)
                                        (point-charpos point)
                                        (syntax-table-block-comment-pairs
                                         (current-syntax))))

  (defun syntax-end-block-comment-p (point)
    (syntax-end-block-syntax-string-p (line-string point)
                                      (point-charpos point)
                                      (syntax-table-block-comment-pairs
                                       (current-syntax))))

  (defun syntax-start-block-string-p (point)
    (syntax-start-block-syntax-string-p (line-string point)
                                        (point-charpos point)
                                        (syntax-table-block-string-pairs
                                         (current-syntax))))

  (defun syntax-end-block-string-p (point)
    (syntax-end-block-syntax-string-p (line-string point)
                                      (point-charpos point)
                                      (syntax-table-block-string-pairs
                                       (current-syntax)))))


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

(defun syntax-escape-point-p (point offset)
  (let ((count 0))
    (loop :with string := (line-string point)
          :for i :downfrom (+ (1- (point-charpos point)) offset) :to 0
          :do (if (syntax-escape-char-p (schar string i))
                  (incf count)
                  (return)))
    (when (oddp count)
      count)))

(defun inline-line-comment-p (point)
  (with-point ((p point))
    (let ((limit)
          (line (point-line point)))
      (let (n pair c)
        (loop :while (eq line (point-line p))
              :do (cond ((multiple-value-setq (n pair) (syntax-end-block-comment-p p))
                         (unless limit (setf limit (line-start (copy-point point :temporary))))
                         (let ((regex (%create-pair-regex pair)))
                           (character-offset p (- n))
                           (loop :with depth := 1
                                 :do (cond ((not (search-backward-regexp p regex limit))
                                            (return-from inline-line-comment-p nil))
                                           ((match-string-at p (car pair))
                                            (when (= 0 (decf depth))
                                              (return)))
                                           (t
                                            (incf depth))))))
                        ((multiple-value-setq (n pair) (syntax-end-block-string-p p))
                         (unless limit (setf limit (line-start (copy-point point :temporary))))
                         (if (search-backward p (car pair) limit)
                             (return)
                             (return-from inline-line-comment-p nil)))
                        (t
                         (unless (character-offset p -1)
                           (return))
                         (cond
                           ((let ((offset (syntax-escape-point-p p 0)))
                              (when offset
                                (character-offset p (- offset)))))
                           ((syntax-line-comment-p p)
                            (loop
                              (when (start-line-p p)
                                (return-from inline-line-comment-p p))
                              (character-offset p -1)
                              (unless (syntax-line-comment-p p)
                                (return-from inline-line-comment-p
                                  (character-offset p 1)))))
                           ((or (syntax-string-quote-char-p (setf c (character-at p)))
                                (syntax-fence-char-p c))
                            (let ((quote-char c))
                              (loop
                                (character-offset p -1)
                                (when (start-line-p p)
                                  (return-from inline-line-comment-p nil))
                                (cond
                                  ((let ((offset (syntax-escape-point-p p 0)))
                                     (when offset
                                       (character-offset p (1+ (- offset))))))
                                  ((char= quote-char (character-at p))
                                   (return))))))))))))))

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

(defun %skip-block-comment-backward (point)
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
        (values nil t))))

(defun %skip-comment-backward (point)
  (multiple-value-bind (p win)
      (%skip-block-comment-backward point)
    (cond ((not win)
           (values nil nil))
          ((null p)
           (let ((p (inline-line-comment-p point)))
             (if p
                 (values (move-point point p) t)
                 (values nil t))))
          (t
           (values point win)))))

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
    (if (inline-line-comment-p point)
        (skip-chars-backward point #'syntax-space-char-p)
        (loop
          (skip-chars-backward point #'syntax-space-char-p)
          (multiple-value-bind (result success)
              (%skip-comment-backward point)
            (unless result
              (return success)))))))

(defun %skip-symbol-forward (point)
  (loop :for c := (character-at point 0)
        :do (cond ((syntax-escape-char-p c)
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
        :do (let ((skip-count (syntax-escape-point-p point -1)))
              (cond (skip-count
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

(defun %skip-quote-forward (point)
  (loop :with quote-char := (character-at point 0)
        :do (unless (character-offset point 1)
              (return nil))
            (let ((c (character-at point)))
              (cond ((syntax-escape-char-p c)
                     (character-offset point 1))
                    ((eql c quote-char)
                     (character-offset point 1)
                     (return point))))))

(defun %skip-quote-backward (point)
  (character-offset point -1)
  (loop :with quote-char := (character-at point)
        :do (unless (character-offset point -1)
              (return nil))
            (if (syntax-escape-point-p point 0)
                (character-offset point -1)
                (let ((c (character-at point)))
                  (cond ((eql c quote-char)
                         (return point)))))))

(defun %skip-string-forward (point)
  (%skip-quote-forward point))

(defun %skip-string-backward (point)
  (%skip-quote-backward point))

(defun %skip-fence-forward (point)
  (%skip-quote-forward point))

(defun %skip-fence-backward (point)
  (%skip-quote-backward point))

(defvar *scan-lists-limit-point* nil)

(defun %skip-list-forward (point depth)
  (loop :with paren-stack := '()
        :do (unless (skip-space-and-comment-forward point)
              (return nil))
            (when (if *scan-lists-limit-point*
                      (point<= *scan-lists-limit-point* point)
                      (end-buffer-p point))
              (return nil))
            (let ((c (character-at point 0))
                  n pair)
              (cond ((multiple-value-setq (n pair) (syntax-start-block-string-p point))
                     (character-offset point n)
                     (unless (search-forward point (cdr pair))
                       (return nil)))
                    ((syntax-open-paren-char-p c)
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
  (loop :with paren-stack := '()
        :do (when (if *scan-lists-limit-point*
                      (point<= point *scan-lists-limit-point*)
                      (start-buffer-p point))
              (return nil))
            (let ((c (character-at point -1))
                  n pair)
              (cond ((syntax-escape-point-p point -1)
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
                    ((multiple-value-setq (n pair) (syntax-end-block-string-p point))
                     (character-offset point (- n))
                     (unless (search-backward point (car pair))
                       (return nil)))
                    ((syntax-string-quote-char-p c)
                     (%skip-string-backward point))
                    ((syntax-fence-char-p c)
                     (%skip-fence-backward point))
                    (t
                     (multiple-value-bind (p win)
                         (%skip-block-comment-backward point)
                       (unless win
                         (return nil))
                       (unless p
                         (character-offset point -1))))))
            (when (end-line-p point)
              (let ((p (inline-line-comment-p point)))
                (when p (move-point point p))))))

(defun %form-offset-positive (point)
  (skip-space-and-comment-forward point)
  (when (end-buffer-p point)
    (return-from %form-offset-positive nil))
  (syntax-skip-expr-prefix-forward point)
  (skip-chars-forward point #'syntax-expr-prefix-char-p)
  (unless (end-buffer-p point)
    (let ((c (character-at point))
          n pair)
      (let ((point
              (cond ((multiple-value-setq (n pair) (syntax-start-block-string-p point))
                     (character-offset point n)
                     (if (search-forward point (cdr pair))
                         point
                         nil))
                    ((syntax-open-paren-char-p c)
                     (%skip-list-forward point 0))
                    ((or (syntax-symbol-char-p c)
                         (syntax-escape-char-p c))
                     (%skip-symbol-forward point))
                    ((syntax-expr-prefix-char-p c)
                     (character-offset point 1))
                    ((syntax-closed-paren-char-p c)
                     nil)
                    ((syntax-string-quote-char-p c)
                     (%skip-string-forward point))
                    ((syntax-fence-char-p c)
                     (%skip-fence-forward point))
                    (t
                     (character-offset point 1)))))
        (when point
          (when (syntax-table-expr-suffix-chars (current-syntax))
            (skip-chars-forward point
                                (syntax-table-expr-suffix-chars
                                 (current-syntax))))
          point)))))

(defun %form-offset-negative (point)
  (skip-space-and-comment-backward point)
  (when (start-buffer-p point)
    (return-from %form-offset-negative nil))
  (when (syntax-table-expr-suffix-chars (current-syntax))
    (skip-chars-backward point (syntax-table-expr-suffix-chars (current-syntax))))
  (let ((c (character-at point -1))
        (escape-point-p (syntax-escape-point-p point -1))
        n pair)
    (prog1 (cond ((and (syntax-closed-paren-char-p c)
                       (not escape-point-p))
                  (%skip-list-backward point 0))
                 ((or (syntax-symbol-char-p c)
                      (syntax-escape-char-p c)
                      (syntax-expr-prefix-char-p c)
                      escape-point-p)
                  (%skip-symbol-backward point))
                 ((syntax-open-paren-char-p c)
                  nil)
                 ((multiple-value-setq (n pair) (syntax-end-block-string-p point))
                  (character-offset point (- n))
                  (if (search-backward point (car pair))
                      point
                      nil))
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

(defun scan-lists (point n depth &optional no-errors limit-point)
  (with-point-syntax point
    (with-point ((curr point))
      (let ((*scan-lists-limit-point* limit-point))
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
          (move-point point curr))))))

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

(defun skip-symbol-forward (point)
  (with-point-syntax point
    (skip-chars-forward point #'syntax-symbol-char-p)))

(defun skip-symbol-backward (point)
  (with-point-syntax point
    (skip-chars-backward point #'syntax-symbol-char-p)))

(defun symbol-string-at-point (point)
  (with-point-syntax point
    (with-point ((point point))
      (skip-chars-backward point #'syntax-symbol-char-p)
      (unless (syntax-symbol-char-p (character-at point))
        (return-from symbol-string-at-point nil))
      (with-point ((start point))
        (skip-chars-forward point #'syntax-symbol-char-p)
        (points-to-string start point)))))


(defstruct pps-state
  type
  token-start-point
  end-char
  block-comment-depth
  block-pair
  paren-stack
  (paren-depth 0))

(defun parse-partial-sexp (from to &optional state comment-stop)
  (assert (eq (point-buffer from)
              (point-buffer to)))
  (unless state (setf state (make-pps-state)))
  (with-point-syntax from
    (let ((p from)
          (type (pps-state-type state))
          (token-start-point (pps-state-token-start-point state))
          (end-char (pps-state-end-char state))
          (block-comment-depth (pps-state-block-comment-depth state))
          (block-pair (pps-state-block-pair state))
          (paren-stack (pps-state-paren-stack state))
          (paren-depth (pps-state-paren-depth state)))
      (flet ((update-token-start-point (p)
               (if token-start-point
                   (move-point token-start-point p)
                   (setf token-start-point (copy-point p :temporary)))))
        (block outer
          (loop
            (case type
              (:block-string
               ;; TODO: a203e4cf9のブロックコメントの修正をここにもすること
               (cond
                 ((search-forward p (cdr block-pair) to)
                  (setf block-pair nil)
                  (setf type nil)
                  (setf token-start-point nil))
                 (t
                  (move-point p to)
                  (return-from outer))))
              ((:string :fence)
               (loop
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
              (:block-comment
               (let ((regex (%create-pair-regex block-pair)))
                 (loop
                   (unless (search-forward-regexp p regex to)
                     (move-point p to)
                     (return-from outer))
                   (let ((end-block-p (match-string-at p (cdr block-pair))))
                     (let ((offset
                             (length (if end-block-p
                                         (cdr block-pair)
                                         (car block-pair)))))
                       (character-offset p offset)
                       (when (point< to p)
                         (character-offset p (- offset))
                         (return-from outer)))
                     (cond
                       (end-block-p
                        (when (= 0 (decf block-comment-depth))
                          (setf block-comment-depth nil)
                          (setf block-pair nil)
                          (setf type nil)
                          (setf token-start-point nil)
                          (return p)))
                       (t
                        (incf block-comment-depth)))))))
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
                     ((multiple-value-bind (n pair)
                          (syntax-start-block-string-p p)
                        (when n
                          (update-token-start-point p)
                          (setf type :block-string)
                          (setf block-pair pair)
                          (character-offset p n)
                          (return))))
                     ((or (syntax-string-quote-char-p c)
                          (syntax-fence-char-p c))
                      (setf type (if (syntax-string-quote-char-p c)
                                     :string
                                     :fence))
                      (setf end-char c)
                      (update-token-start-point p)
                      (character-offset p 1)
                      (return))
                     ((multiple-value-bind (n pair)
                          (syntax-start-block-comment-p p)
                        (when n
                          (when comment-stop
                            (return-from outer))
                          (update-token-start-point p)
                          (setf type :block-comment)
                          (setf block-comment-depth 1)
                          (setf block-pair pair)
                          (character-offset p n)
                          (return))))
                     ((syntax-line-comment-p p)
                      (when comment-stop
                        (return-from outer))
                      (setf type :line-comment)
                      (update-token-start-point p)
                      (return))
                     ((syntax-open-paren-char-p c)
                      (incf paren-depth)
                      (push c paren-stack))
                     ((syntax-closed-paren-char-p c)
                      (decf paren-depth)
                      (when (syntax-equal-paren-p c (car paren-stack))
                        (pop paren-stack))))
                   (character-offset p 1)))))))
        (without-interrupts
          (setf (pps-state-type state) type
                (pps-state-token-start-point state) token-start-point
                (pps-state-end-char state) end-char
                (pps-state-block-comment-depth state) block-comment-depth
                (pps-state-block-pair state) block-pair
                (pps-state-paren-stack state) paren-stack
                (pps-state-paren-depth state) paren-depth))
        state))))

(define-editor-variable syntax-ppss-cache nil)

(defmacro syntax-ppss-cache (buffer)
  `(variable-value 'syntax-ppss-cache :buffer ,buffer))

(flet ((cache-point (cache) (car cache))
       (cache-state (cache) (cdr cache)))
  (defun syntax-ppss (point)
    (let* ((buffer (point-buffer point))
           (cache-list (syntax-ppss-cache buffer))
           state)
      (do ((rest cache-list (cdr rest))
           (prev nil rest))
          ((null rest)
           (setf state (parse-partial-sexp (copy-point (buffer-start-point buffer) :temporary)
                                           point))
           (let ((new-rest (list (cons (copy-point point :temporary) state))))
             (if prev
                 (setf (cdr prev) new-rest)
                 (setf cache-list new-rest))))
        (cond ((point= point (cache-point (car rest)))
               (setf state (cache-state (car rest)))
               (return))
              ((point> point (cache-point (car rest)))
               (setf state (parse-partial-sexp (copy-point (cache-point (car rest)) :temporary)
                                               point
                                               (copy-pps-state (cache-state (car rest)))))
               (let ((new-rest (cons (cons (copy-point point :temporary) state)
                                     rest)))
                 (if prev
                     (setf (cdr prev) new-rest)
                     (setf cache-list new-rest))
                 (return)))))
      (add-hook (variable-value 'before-change-functions :buffer buffer)
                'syntax-ppss-clear-cache)
      (setf (syntax-ppss-cache buffer)
            cache-list)
      state))

  (defun syntax-ppss-clear-cache (point &rest ignore-args)
    (declare (ignore ignore-args))
    (if (null point)
        (setf (syntax-ppss-cache point) nil)
        (let ((list (member-if (lambda (cache)
                                 (point< (cache-point cache) point))
                               (syntax-ppss-cache point))))
          (setf (syntax-ppss-cache point) list))))

  (defun check-ppss-cache (buffer)
    (loop :for prev := nil :then (cache-point cache)
          :for cache :in (syntax-ppss-cache buffer)
          :do (assert (line-alive-p (point-line (cache-point cache))))
              (when prev
                (assert (point< (cache-point cache) prev))))))

(defun pps-state-string-p (state)
  (member (pps-state-type state) '(:block-string :string)))

(defun pps-state-comment-p (state)
  (member (pps-state-type state) '(:line-comment :block-comment)))

(defun pps-state-string-or-comment-p (state)
  (member (pps-state-type state)
          '(:block-string
            :string
            :line-comment
            :block-comment)))

(defun in-string-p (point)
  (let ((state (syntax-ppss point)))
    (pps-state-string-p state)))

(defun in-comment-p (point)
  (let ((state (syntax-ppss point)))
    (pps-state-comment-p state)))

(defun in-string-or-comment-p (point)
  (let ((state (syntax-ppss point)))
    (pps-state-string-or-comment-p state)))

(defun maybe-beginning-of-string (point)
  (let ((state (syntax-ppss point)))
    (when (member (pps-state-type state) '(:block-string :string))
      (move-point point (pps-state-token-start-point state)))))

(defun maybe-beginning-of-comment (point)
  (let ((state (syntax-ppss point)))
    (when (member (pps-state-type state) '(:line-comment :block-comment))
      (move-point point (pps-state-token-start-point state)))))

(defun maybe-beginning-of-string-or-comment (point)
  (let ((state (syntax-ppss point)))
    (when (member (pps-state-type state)
                  '(:block-string
                    :string
                    :line-comment
                    :block-comment))
      (move-point point (pps-state-token-start-point state)))))
