(in-package :lem-base)

(export '(syntax-string-attribute
          syntax-comment-attribute
          syntax-keyword-attribute
          syntax-constant-attribute
          syntax-function-name-attribute
          syntax-variable-attribute
          syntax-type-attribute
          *global-syntax-highlight*
          enable-syntax-highlight
          enable-syntax-highlight-p
          make-regex-matcher
          syntax-add-match
          syntax-add-region
          syntax-scan-range))

(define-editor-variable enable-syntax-highlight nil)
(defvar *global-syntax-highlight* t)

(defun make-regex-matcher (thing)
  (ppcre:create-scanner thing))

(defclass syntax ()
  ((attribute
    :initarg :attribute
    :initform 0
    :reader syntax-attribute)))

(defclass syntax-region (syntax)
  ((start
    :initarg :start
    :reader syntax-region-start)
   (end
    :initarg :end
    :reader syntax-region-end)))

(defclass syntax-match (syntax)
  ((test
    :initarg :test
    :initform nil
    :reader syntax-match-test)
   (test-symbol
    :initarg :test-symbol
    :initform nil
    :reader syntax-match-test-symbol)
   (matched-symbol
    :initarg :matched-symbol
    :initform nil
    :reader syntax-match-matched-symbol)
   (move-action
    :initarg :move-action
    :initform nil
    :reader syntax-match-move-action)))

(defun syntax-add-match (syntax-table test
                                      &key test-symbol attribute
                                      matched-symbol move-action)
  (push (make-instance 'syntax-match
                       :test test
                       :test-symbol test-symbol
                       :attribute attribute
                       :matched-symbol matched-symbol
                       :move-action move-action)
        (syntax-table-patterns syntax-table))
  t)

(defun syntax-add-region (syntax-table start end &key attribute)
  (push (make-instance 'syntax-region
                       :start start
                       :end end
                       :attribute attribute)
        (syntax-table-patterns syntax-table)))

(defun enable-syntax-highlight-p (buffer)
  (and *global-syntax-highlight*
       (variable-value 'enable-syntax-highlight :buffer buffer)))

(defvar *syntax-scan-limit*)
(defvar *syntax-symbol-lifetimes* nil)

(defun set-syntax-context (line x)
  (if (line-%syntax-context line)
      (setf (car (line-%syntax-context line)) x)
      (setf (line-%syntax-context line) (cons x nil))))

(defun get-syntax-context (line)
  (car (line-%syntax-context line)))

(defun set-syntax-lifetimes (line lifetimes)
  (if (line-%syntax-context line)
      (setf (cdr (line-%syntax-context line)) lifetimes)
      (setf (line-%syntax-context line) (cons nil lifetimes))))

(defun get-syntax-lifetimes (line)
  (cdr (line-%syntax-context line)))

(defun syntax-update-symbol-lifetimes ()
  (setq *syntax-symbol-lifetimes*
        (loop :for (symbol . lifetime) :in *syntax-symbol-lifetimes*
	   :when (/= 0 lifetime)
	   :collect (cons symbol (1- lifetime)))))

(defun syntax-test-match-p (syntax-test point)
  (let ((string (line-string point)))
    (multiple-value-bind (start end)
        (ppcre:scan syntax-test string :start (point-charpos point))
      (when (and start
                 (= (point-charpos point) start))
        (line-offset point 0 end)
        point))))

(defun syntax-scan-region (region point start-charpos)
  (do ((start-charpos start-charpos 0)) (nil)
    (loop
      (cond ((syntax-escape-char-p (character-at point 0))
             (character-offset point 1)
             (when (end-line-p point)
               (return)))
            ((syntax-test-match-p (syntax-region-end region) point)
             (line-add-property (point-line point)
                                start-charpos (point-charpos point)
                                :attribute (syntax-attribute region)
                                nil)
             (set-syntax-context (point-line point) nil)
             (return-from syntax-scan-region (values point t)))
            ((end-line-p point)
             (return)))
      (character-offset point 1))
    (line-add-property (point-line point)
                       start-charpos (line-length (point-line point))
                       :attribute (syntax-attribute region)
                       t)
    (set-syntax-context (point-line point) region)
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
              (set-syntax-context (point-line cur) syntax)
              (line-offset cur 1))
        (set-syntax-context (point-line cur) 'end-move-action)
        (when (syntax-attribute syntax)
          (put-text-property start end :attribute (syntax-attribute syntax)))
        end))))

(defun syntax-scan-token-test (syntax point)
  (etypecase syntax
    (syntax-region
     (let ((start-charpos (point-charpos point))
           (point (syntax-test-match-p (syntax-region-start syntax) point)))
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
                         1)
                   *syntax-symbol-lifetimes*))
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
         (syntax (and prev (get-syntax-context prev))))
    (if (null syntax)
        (set-syntax-context line nil)
        (cond
          ((typep syntax 'syntax-region)
           (syntax-scan-region syntax point (point-charpos point)))
          ((typep syntax 'syntax)
           (cond ((eq (get-syntax-context line) 'end-move-action)
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
          (t (set-syntax-context line nil))))))

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
                      ((dolist (syn (syntax-table-patterns (current-syntax)))
                         (when (syntax-scan-token-test syn point)
                           (return t))))
                      (t
                       (when (= 0 (skip-chars-forward point #'syntax-symbol-char-p))
                         (character-offset point 1))
                       t))
            (syntax-update-symbol-lifetimes)))
    (set-syntax-lifetimes (point-line point) *syntax-symbol-lifetimes*)
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
               (and prev (get-syntax-lifetimes prev)))))
        (with-point ((start start)
                     (end end))
          (line-start start)
          (line-end end)
          (loop
            (line-clear-property (point-line start) :attribute)
            (syntax-scan-line start end)
            (when (point<= end start)
              (return start))))))))
