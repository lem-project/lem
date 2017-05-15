(in-package :lem-base)

(export '(make-tmlanguage
          make-regex-matcher
          make-tm-match
          make-tm-region
          add-tm-pattern
          add-tm-repository
          make-tm-patterns
          make-tm-name))

;;; TODO
;;; begin/endルールでbeginのキャプチャをendで使えるようにする

(defclass tmlanguage (syntax-parser)
  ((patterns
    :initarg :patterns
    :accessor tmlanguage-patterns)
   (repository
    :initarg :repository
    :accessor tmlanguage-repository)))

(defclass tm-rule ()
  ((name
    :initarg :name
    :initform 0
    :reader tm-rule-name)))

(defclass tm-region (tm-rule)
  ((begin
    :initarg :begin
    :reader tm-region-begin)
   (end
    :initarg :end
    :reader tm-region-end)
   (content-name
    :initarg :content-name
    :initform nil
    :reader tm-region-content-name)
   (patterns
    :initarg :patterns
    :initform nil
    :reader tm-region-patterns)))

(defclass tm-match (tm-rule)
  ((matcher
    :initarg :matcher
    :initform nil
    :reader tm-match-matcher)
   (captures
    :initarg :captures
    :initform nil
    :reader tm-match-captures)
   (move-action
    :initarg :move-action
    :initform nil
    :reader tm-match-move-action)))

(defclass tm-include ()
  ((spec
    :initarg :spec
    :reader tm-include-spec)
   (refer
    :initarg :refer
    :initform nil
    :reader tm-include-refer)))

(defclass tm-patterns ()
  ((patterns
    :initarg :patterns
    :accessor patterns)))

(defun make-tmlanguage ()
  (make-instance 'tmlanguage
                 :patterns (make-tm-patterns)
                 :repository (make-hash-table :test 'equal)))

(defun make-tm-match (matcher &key name captures move-action)
  (make-instance 'tm-match
                 :matcher matcher
                 :name name
                 :captures captures
                 :move-action move-action))

(defun make-tm-region (begin-matcher end-matcher
                       &key name content-name (patterns (make-tm-patterns)))
  (make-instance 'tm-region
                 :begin begin-matcher
                 :end end-matcher
                 :name name
                 :content-name content-name
                 :patterns patterns))

(defun make-tm-include (spec)
  (cond ((string= spec "$self")
         (make-instance 'tm-include :type :self))
        ((char= #\# (schar spec 0))
         (make-instance 'tm-include :type :repository :refer (subseq spec 1)))
        (t
         (error "unsupported spec: ~A" spec))))

(defun make-regex-matcher (regex)
  (ppcre:create-scanner regex))

(defun make-tm-patterns (&rest patterns)
  (make-instance 'tm-patterns :patterns patterns))

(defun make-tm-name (name)
  name)

(defun add-tm-pattern (tmlanguage pattern)
  (push pattern (patterns (tmlanguage-patterns tmlanguage))))

(defun add-tm-repository (tmlanguage name patterns)
  (setf (gethash name (tmlanguage-repository tmlanguage))
        patterns))

(defmethod %syntax-scan-region ((tmlanguage tmlanguage) start end)
  (tm-syntax-scan-region start end))


(defun set-syntax-context (line x)
  (setf (line-%syntax-context line) x))

(defun get-syntax-context (line)
  (line-%syntax-context line))

(defun tm-ahead-matcher (rule)
  (etypecase rule
    (tm-region
     (tm-region-begin rule))
    (tm-match
     (tm-match-matcher rule))))

(defun tm-ahead-match (rule matcher string start end)
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan matcher string :start start :end (or end (length string)))
    (when start
      (vector rule start end reg-starts reg-ends))))

(defun tm-result-rule       (result) (aref result 0))
(defun tm-result-start      (result) (aref result 1))
(defun tm-result-end        (result) (aref result 2))
(defun tm-result-reg-starts (result) (aref result 3))
(defun tm-result-reg-ends   (result) (aref result 4))

(defun tm-result= (result1 result2)
  (and (eq (tm-result-rule result1)
           (tm-result-rule result2))
       (= (tm-result-start result1)
          (tm-result-start result2))
       (= (tm-result-end result1)
          (tm-result-end result2))
       (equal (tm-result-reg-starts result1)
              (tm-result-reg-starts result2))
       (equal (tm-result-reg-ends result1)
              (tm-result-reg-ends result2))))

(defun tm-get-results-from-patterns (patterns string start end)
  (loop :for rule :in (patterns patterns)
        :for result := (tm-ahead-match rule (tm-ahead-matcher rule) string start end)
        :when result
        :collect result))

(defun tm-best-result (results)
  (let ((min)
        (best))
    (dolist (result results)
      (when (and result (or (null min) (> min (tm-result-start result))))
        (setf min (tm-result-start result))
        (setf best result)))
    best))

(defun tm-recompute-results (results string start end)
  (loop :for rest-results :on results
        :for result := (car rest-results)
        :do (when (and result
                       (>= start (tm-result-start result)))
              (let ((new-result
                     (tm-ahead-match (tm-result-rule result)
                                     (tm-ahead-matcher (tm-result-rule result))
                                     string
                                     start
                                     end)))
                (setf (car rest-results)
                      new-result)))))

(defun tm-apply-region (rule point begin-result end)
  (let ((start1 (if begin-result (tm-result-start begin-result) 0))
        (start2 (if begin-result (tm-result-end begin-result) 0)))
    (let* ((end-result
            (tm-ahead-match rule
                            (tm-region-end rule)
                            (line-string point)
                            start2
                            end))
           (results
            (cons end-result
                  (tm-get-results-from-patterns (tm-region-patterns rule)
                                                (line-string point)
                                                start2
                                                nil))))
      (loop
        (let ((best (tm-best-result results)))
          (cond ((or (null end-result)
                     (null best))
                 (line-add-property (point-line point)
                                    start1
                                    (line-length (point-line point))
                                    :attribute (tm-rule-name rule)
                                    t)
                 (alexandria:when-let (content-name (tm-region-content-name rule))
                   (line-add-property (point-line point)
                                      start2
                                      (line-length (point-line point))
                                      :attribute content-name
                                      t))
                 (set-syntax-context (point-line point) rule)
                 (line-end point)
                 (return))
                ((tm-result= best end-result)
                 (line-add-property (point-line point)
                                    start1
                                    (tm-result-end end-result)
                                    :attribute (tm-rule-name rule)
                                    nil)
                 (alexandria:when-let (content-name (tm-region-content-name rule))
                   (line-add-property (point-line point)
                                      start1
                                      (tm-result-start end-result)
                                      :attribute content-name
                                      nil))
                 (set-syntax-context (point-line point) nil)
                 (line-offset point 0 (tm-result-end end-result))
                 (return))
                (t
                 (line-offset point 0 (tm-result-end best))
                 (tm-recompute-results results
                                       (line-string point)
                                       (point-charpos point)
                                       end)
                 (dolist (result results)
                   (when (and result (eq (tm-result-rule result) rule))
                     (setf end-result result)
                     (return))))))))))

(defun tm-move-action (rule point allow-multiline)
  (with-point ((start point)
               (end point))
    (let ((end (funcall (tm-match-move-action rule) end)))
      (when (and end (point< point end))
        (loop :until (same-line-p point end)
              :do
              (set-syntax-context (point-line point) rule)
              (line-offset point 1))
        (set-syntax-context (point-line point) 'end-move-action)
        (alexandria:when-let ((attribute (tm-rule-name rule)))
          (put-text-property start end :attribute attribute))
        (cond (allow-multiline
               (move-point point end))
              ((same-line-p start end)
               (move-point point end))
              (t
               (line-end (move-point point start))))))))

(defun tm-apply-match-in-capture (point capture start end)
  (typecase capture
    (tm-patterns
     (tm-scan-line point capture start end))
    (otherwise
     (line-add-property (point-line point) start end :attribute capture nil))))

(defun tm-apply-match (rule point result)
  (let ((start (tm-result-start result))
        (end (tm-result-end result))
        (reg-starts (tm-result-reg-starts result))
        (reg-ends (tm-result-reg-ends result))
        (captures (tm-match-captures rule)))
    (line-add-property (point-line point) start end :attribute (tm-rule-name rule) nil)
    (when (and captures (< 0 (length captures)))
      (alexandria:when-let (cap0 (aref captures 0))
        (tm-apply-match-in-capture point cap0 start end))
      (loop :for reg-start-index :from 0 :below (length reg-starts)
            :for reg-end-index :from 0 :below (length reg-ends)
            :for captures-index :from 1 :below (length captures)
            :for reg-start := (aref reg-starts reg-start-index)
            :for reg-end := (aref reg-ends reg-end-index)
            :for cap := (aref captures captures-index)
            :do (when cap
                  (tm-apply-match-in-capture point cap reg-start reg-end))))
    (cond ((tm-match-move-action rule)
           (line-offset point 0 start)
           (or (tm-move-action rule point nil)
               (line-offset point 0 end)))
          (t
           (line-offset point 0 end)))))

(defun tm-apply-result (point result end)
  (let ((rule (tm-result-rule result)))
    (etypecase rule
      (tm-region
       (tm-apply-region rule point result end))
      (tm-match
       (tm-apply-match rule point result)))))

(defun tm-continue-prev-line (point)
  (let* ((line (point-line point))
         (prev (line-prev line))
         (context (and prev (get-syntax-context prev))))
    (cond ((null context)
           (set-syntax-context line nil))
          ((typep context 'tm-region)
           (tm-apply-region context point nil nil))
          ((typep context 'tm-rule)
           (cond ((eq (get-syntax-context line) 'end-move-action)
                  (with-point ((p point))
                    (previous-single-property-change p :attribute)
                    (let ((goal (tm-move-action context p t)))
                      (when goal
                        (move-point point goal)))))
                 (t
                  (line-add-property (point-line point)
                                     0 (line-length line)
                                     :attribute (tm-rule-name context)
                                     t)
                  (line-end point))))
          (t
           (set-syntax-context line nil)))))

(defun tm-scan-line (point patterns start end)
  (let ((results (tm-get-results-from-patterns patterns (line-string point) start end))
        (old-linenumber (line-number-at-point point)))
    (loop
      (let ((best (tm-best-result results)))
        (unless best (return))
        (tm-apply-result point best end)
        (tm-recompute-results results
                              (line-string point)
                              (point-charpos point)
                              end)))
    (assert (= old-linenumber (line-number-at-point point)))
    (when (and end (< end (point-charpos point)))
      (line-offset point 0 end))))

(defun tm-syntax-scan-line (point)
  (tm-continue-prev-line point)
  (tm-scan-line point
                (tmlanguage-patterns (current-syntax-parser))
                (point-charpos point)
                nil)
  (line-offset point 1))

(defun tm-syntax-scan-region (start end)
  (loop
    (line-clear-property (point-line start) :attribute)
    (unless (tm-syntax-scan-line start)
      (return start))
    (when (point<= end start)
      (return start))))
