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

(defun tm-apply-region (syntax point begin-result)
  (declare (type point point))
  (let ((end-result
         (tm-ahead-match syntax
                         (syntax-region-end syntax)
                         (line-string point)
                         (if begin-result
                             (tm-result-end begin-result)
                             0))))
    (cond (end-result
           (line-add-property (point-line point)
                              (if begin-result
                                  (tm-result-start begin-result)
                                  0)
                              (tm-result-end end-result)
                              :attribute (syntax-attribute syntax)
                              nil)
           (line-offset point 0 (tm-result-end end-result)))
          (t
           (line-add-property (point-line point)
                              (if begin-result
                                  (tm-result-start begin-result)
                                  0)
                              (line-length (point-line point))
                              :attribute (syntax-attribute syntax)
                              t)
           (set-syntax-context (point-line point) syntax)
           (line-end point)))))

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

(defun tm-find-ahead-results (results)
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
         (loop :for syntax :in (syntax-table-patterns (current-syntax))
               :for result := (tm-ahead-match syntax
                                              (tm-ahead-matcher syntax)
                                              (line-string point)
                                              (point-charpos point))
               :when result
               :collect result)))
    (loop :for best := (tm-find-ahead-results results)
          :while best :do
          (tm-apply-result point best)
          (tm-recompute-results results
                                (line-string point)
                                (point-charpos point))))
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
