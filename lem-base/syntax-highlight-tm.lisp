(in-package :lem-base)

(defun tm-ahead-matcher (syntax)
  (etypecase syntax
    (syntax-region
     (syntax-region-begin syntax))
    (syntax-match
     (syntax-match-matcher syntax))))

(defun tm-ahead-match (syntax syntax-test string start)
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan syntax-test string :start start)
    (when start
      (vector syntax start end reg-starts reg-ends))))

(defun tm-result-syntax     (result) (aref result 0))
(defun tm-result-start      (result) (aref result 1))
(defun tm-result-end        (result) (aref result 2))
(defun tm-result-reg-starts (result) (aref result 3))
(defun tm-result-reg-ends   (result) (aref result 4))

(defun tm-result= (result1 result2)
  (and (eq (tm-result-syntax result1)
           (tm-result-syntax result2))
       (= (tm-result-start result1)
          (tm-result-start result2))
       (= (tm-result-end result1)
          (tm-result-end result2))
       (equal (tm-result-reg-starts result1)
              (tm-result-reg-starts result2))
       (equal (tm-result-reg-ends result1)
              (tm-result-reg-ends result2))))

(defun tm-get-results-from-patterns (patterns string start)
  (loop :for syntax :in patterns
        :for result := (tm-ahead-match syntax (tm-ahead-matcher syntax) string start)
        :when result
        :collect result))

(defun tm-best-result (results)
  (let ((min)
        (best))
    (loop :for result :in results
          :do (when (and result (or (null min) (> min (tm-result-start result))))
                (setf min (tm-result-start result))
                (setf best result)))
    best))

(defun tm-recompute-results (results string i)
  (loop :for rest-results :on results
        :for result := (car rest-results)
        :do (when (and result
                       (>= i (tm-result-start result)))
              (let ((new-result
                     (tm-ahead-match (tm-result-syntax result)
                                     (tm-ahead-matcher (tm-result-syntax result))
                                     string
                                     i)))
                (setf (car rest-results)
                      new-result)))))

(defun tm-apply-region (syntax point begin-result)
  (let ((start1 (if begin-result (tm-result-start begin-result) 0))
        (start2 (if begin-result (tm-result-end begin-result) 0)))
    (let* ((end-result
            (tm-ahead-match syntax
                            (syntax-region-end syntax)
                            (line-string point)
                            start2))
           (results
            (cons end-result
                  (tm-get-results-from-patterns (syntax-region-patterns syntax)
                                                (line-string point)
                                                start2))))
      (loop :for $count :from 0 :do
        (let ((best (tm-best-result results)))
          (cond ((null end-result)
                 (line-add-property (point-line point)
                                    start1
                                    (line-length (point-line point))
                                    :attribute (syntax-attribute syntax)
                                    t)
                 (set-syntax-context (point-line point) syntax)
                 (line-end point)
                 (return))
                ((tm-result= best end-result)
                 (line-add-property (point-line point)
                                    start1
                                    (tm-result-end end-result)
                                    :attribute (syntax-attribute syntax)
                                    nil)
                 (line-offset point 0 (tm-result-end end-result))
                 (return))
                (t
                 (line-offset point 0 (tm-result-end best))
                 (tm-recompute-results results
                                       (line-string point)
                                       (point-charpos point))
                 (loop :for result :in results
                       :do (when (and result (eq (tm-result-syntax result) syntax))
                             (setf end-result result)
                             (return))))))))))

(defun tm-apply-result (point result)
  (let ((syntax (tm-result-syntax result)))
    (etypecase syntax
      (syntax-region
       (tm-apply-region syntax point result))
      (syntax-match
       (let ((start (tm-result-start result))
             (end (tm-result-end result))
             (reg-starts (tm-result-reg-starts result))
             (reg-ends (tm-result-reg-ends result))
             (captures (syntax-match-captures syntax)))
         (line-add-property (point-line point)
                            start end
                            :attribute (syntax-attribute syntax) nil)
         (when captures
           (loop :for reg-start-index :from 0 :below (length reg-starts)
                 :for reg-end-index :from 0 :below (length reg-ends)
                 :for captures-index :from 1 :below (length captures)
                 :for reg-start := (aref reg-starts reg-start-index)
                 :for reg-end := (aref reg-ends reg-end-index)
                 :for cap := (aref captures captures-index)
                 :do (when cap
                       (line-add-property (point-line point)
                                          reg-start
                                          reg-end
                                          :attribute cap
                                          nil))))
         (line-offset point 0 end))))))

(defun tm-continue-prev-line (point)
  (let* ((line (point-line point))
         (prev (line-prev line))
         (context (and prev (get-syntax-context prev))))
    (cond ((null context)
           (set-syntax-context line nil))
          ((typep context 'syntax-region)
           (tm-apply-region context point nil))
          ((typep context 'syntax)
           (error "trap"))
          (t
           (set-syntax-context line nil)))))

(defun tm-syntax-scan-line (point)
  (tm-continue-prev-line point)
  (let ((results
         (tm-get-results-from-patterns (syntax-table-patterns (current-syntax))
                                       (line-string point)
                                       (point-charpos point))))
    (loop
      (let ((best (tm-best-result results)))
        (unless best (return))
        (tm-apply-result point best)
        (tm-recompute-results results
                              (line-string point)
                              (point-charpos point)))))
  (line-offset point 1))

(defun tm-syntax-scan-region (start end)
  (assert (eq (point-buffer start)
              (point-buffer end)))
  (let ((buffer (point-buffer start)))
    (when (enable-syntax-highlight-p buffer)
      (let ((*current-syntax*
             (buffer-syntax-table buffer)))
        (with-point ((start start)
                     (end end))
          (line-start start)
          (line-end end)
          (loop
            (line-clear-property (point-line start) :attribute)
            (unless (tm-syntax-scan-line start)
              (return start))
            (when (point<= end start)
              (return start))))))))

(setf *syntax-scan-region-function* 'tm-syntax-scan-region)
