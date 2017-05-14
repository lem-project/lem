(in-package :lem-base)

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

(defun tm-ahead-match (rule matcher string start)
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan matcher string :start start)
    (when start
      (vector rule start end reg-starts reg-ends))))

(defun tm-result-rule     (result) (aref result 0))
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

(defun tm-get-results-from-patterns (patterns string start)
  (loop :for rule :in patterns
        :for result := (tm-ahead-match rule (tm-ahead-matcher rule) string start)
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
                     (tm-ahead-match (tm-result-rule result)
                                     (tm-ahead-matcher (tm-result-rule result))
                                     string
                                     i)))
                (setf (car rest-results)
                      new-result)))))

(defun tm-apply-region (rule point begin-result)
  (let ((start1 (if begin-result (tm-result-start begin-result) 0))
        (start2 (if begin-result (tm-result-end begin-result) 0)))
    (let* ((end-result
            (tm-ahead-match rule
                            (tm-region-end rule)
                            (line-string point)
                            start2))
           (results
            (cons end-result
                  (tm-get-results-from-patterns (tm-region-patterns rule)
                                                (line-string point)
                                                start2))))
      (loop
        (let ((best (tm-best-result results)))
          (cond ((or (null end-result)
                     (null best))
                 (line-add-property (point-line point)
                                    start1
                                    (line-length (point-line point))
                                    :attribute (tm-rule-attribute rule)
                                    t)
                 (set-syntax-context (point-line point) rule)
                 (line-end point)
                 (return))
                ((tm-result= best end-result)
                 (line-add-property (point-line point)
                                    start1
                                    (tm-result-end end-result)
                                    :attribute (tm-rule-attribute rule)
                                    nil)
                 (line-offset point 0 (tm-result-end end-result))
                 (return))
                (t
                 (line-offset point 0 (tm-result-end best))
                 (tm-recompute-results results
                                       (line-string point)
                                       (point-charpos point))
                 (dolist (result results)
                   (when (and result (eq (tm-result-rule result) rule))
                     (setf end-result result)
                     (return))))))))))

(defun tm-move-action (rule point)
  (with-point ((start point)
               (end point))
    (let ((end (funcall (tm-match-move-action rule) end)))
      (when (and end (point< point end))
        (loop :until (same-line-p point end)
              :do
              (set-syntax-context (point-line point) rule)
              (line-offset point 1))
        (set-syntax-context (point-line point) 'end-move-action)
        (uiop:if-let ((attribute (tm-rule-attribute rule)))
          (put-text-property start end :attribute attribute))
        (move-point point end)))))

(defun tm-apply-match (rule point result)
  (let ((start (tm-result-start result))
        (end (tm-result-end result))
        (reg-starts (tm-result-reg-starts result))
        (reg-ends (tm-result-reg-ends result))
        (captures (tm-match-captures rule)))
    (line-add-property (point-line point)
                       start end
                       :attribute (tm-rule-attribute rule) nil)
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
    (cond ((tm-match-move-action rule)
           (line-offset point 0 start)
           (or (tm-move-action rule point)
               (line-offset point 0 end)))
          (t
           (line-offset point 0 end)))))

(defun tm-apply-result (point result)
  (let ((rule (tm-result-rule result)))
    (etypecase rule
      (tm-region
       (tm-apply-region rule point result))
      (tm-match
       (tm-apply-match rule point result)))))

(defun tm-continue-prev-line (point)
  (let* ((line (point-line point))
         (prev (line-prev line))
         (context (and prev (get-syntax-context prev))))
    (cond ((null context)
           (set-syntax-context line nil))
          ((typep context 'tm-region)
           (tm-apply-region context point nil))
          ((typep context 'tm-rule)
           (cond ((eq (get-syntax-context line) 'end-move-action)
                  (with-point ((p point))
                    (previous-single-property-change p :attribute)
                    (let ((goal (tm-move-action context p)))
                      (when goal
                        (move-point point goal)))))
                 (t
                  (line-add-property (point-line point)
                                     0 (line-length line)
                                     :attribute (tm-rule-attribute context)
                                     t)
                  (line-end point))))
          (t
           (set-syntax-context line nil)))))

(defun tm-syntax-scan-line (point)
  (tm-continue-prev-line point)
  (let ((results
         (tm-get-results-from-patterns (tmlanguage-patterns (current-syntax-parser))
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
  (loop
    (line-clear-property (point-line start) :attribute)
    (unless (tm-syntax-scan-line start)
      (return start))
    (when (point<= end start)
      (return start))))
