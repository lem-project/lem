(in-package :lem-base)

(export '(*case-fold-search*
          search-forward
          search-backward
          search-forward-regexp
          search-backward-regexp
          search-forward-symbol
          search-backward-symbol
          looking-at
          match-string-at))

(defvar *case-fold-search* nil)

(defun search-step (point first-search search step move-matched endp)
  (with-point ((start-point point))
    (let ((result
           (let ((res (funcall first-search point)))
             (cond (res
                    (funcall move-matched point res)
                    (not (funcall endp point)))
                   (t
                    (loop :until (funcall endp point) :do
		       (unless (funcall step point)
			 (return nil))
		       (let ((res (funcall search point)))
			 (when res
			   (funcall move-matched point res)
			   (return t)))))))))
      (unless result
        (move-point point start-point))
      result)))

(defun search-forward-endp-function (limit-point)
  (if limit-point
      (lambda (point)
        (or (point<= limit-point point)
            (end-buffer-p point)))
      #'end-buffer-p))

(defun search-backward-endp-function (limit-point)
  (if limit-point
      (lambda (point)
        (point< point limit-point))
      #'start-buffer-p))

(defmacro search* (&rest args)
  `(search ,@args
           :test (if *case-fold-search*
                     #'char=
                     #'char-equal)))

(defun search-forward (point string &optional limit-point)
  (let ((nlines (count #\newline string)))
    (flet ((take-string (point)
             (with-point ((start-point point)
			  (end-point point))
               (points-to-string (line-start start-point)
                                 (line-end (or (line-offset end-point nlines)
                                               (buffer-end end-point)))))))
      (search-step point
                   (lambda (point)
                     (search* string
                              (take-string point)
                              :start2 (point-charpos point)))
                   (lambda (point)
                     (search* string (take-string point)))
                   (lambda (point)
                     (line-offset point 1))
                   (lambda (point charpos)
                     (character-offset (line-start point)
                                       (+ charpos (length string))))
                   (search-forward-endp-function limit-point)))))

(defun search-backward (point string &optional limit-point)
  (let ((nlines (count #\newline string)))
    (flet ((search-from-end (point end-charpos)
             (with-point ((point point))
               (when (line-offset point (- nlines))
                 (search* string
                          (points-to-string
                           (line-start (copy-point point :temporary))
                           (with-point ((point point))
                             (unless (line-offset point nlines)
                               (buffer-end point))
                             (if end-charpos
                                 (character-offset point end-charpos)
                                 (line-end point))))
                          :from-end t)))))
      (let ((end-charpos (point-charpos point)))
        (search-step point
                     (lambda (point)
                       (search-from-end point end-charpos))
                     (lambda (point)
                       (search-from-end point nil))
                     (lambda (point)
                       (line-offset point -1))
                     (lambda (point charpos)
                       (unless (zerop nlines)
                         (line-offset point (- nlines)))
                       (character-offset (line-start point) charpos))
                     (search-backward-endp-function limit-point))))))

(defun search-forward-regexp (point regex &optional limit-point)
  (let ((scanner (ignore-errors (ppcre:create-scanner regex))))
    (when scanner
      (search-step point
                   (lambda (point)
                     (multiple-value-bind (start end)
                         (ppcre:scan scanner
                                     (line-string point)
                                     :start (point-charpos point))
                       (when (and start (<= (point-charpos point) start))
                         end)))
                   (lambda (point)
                     (nth-value 1
                                (ppcre:scan scanner
                                            (line-string point))))
                   (lambda (point)
                     (line-offset point 1))
                   (lambda (point charpos)
                     (character-offset (line-start point) charpos))
                   (search-forward-endp-function limit-point)))))

(defun search-backward-regexp (point regex &optional limit-point)
  (let ((scanner (ignore-errors (ppcre:create-scanner regex))))
    (when scanner
      (search-step point
                   (lambda (point)
                     (let (pos)
                       (ppcre:do-scans (start end reg-starts reg-ends scanner
                                              (line-string point) nil
                                              :end (point-charpos point))
                         (setf pos start))
                       pos))
                   (lambda (point)
                     (let (pos)
                       (ppcre:do-scans (start end reg-starts reg-ends scanner
                                              (line-string point) nil
                                              :start (point-charpos point))
                         (setf pos start))
                       pos))
                   (lambda (point)
                     (line-offset point -1))
                   (lambda (point charpos)
                     (character-offset (line-start point) charpos))
                   (search-backward-endp-function limit-point)))))

(defun search-symbol (string name &key (start 0) (end (length string)) from-end)
  (loop :while (< start end)
     :do (let ((pos (search name string :start2 start :end2 end :from-end from-end)))
	   (when pos
	     (let ((pos2 (+ pos (length name))))
	       (when (and (or (zerop pos)
			      (not (syntax-symbol-char-p (aref string (1- pos)))))
			  (or (>= pos2 (length string))
			      (not (syntax-symbol-char-p (aref string pos2)))))
		 (return (cons pos pos2)))))
           (if from-end
               (setf end (1- (or pos end)))
               (setf start (1+ (or pos start)))))))

(defun search-forward-symbol (point name &optional limit-point)
  (let ((charpos (point-charpos point)))
    (search-step point
                 (lambda (point)
                   (cdr (search-symbol (line-string point) name :start charpos)))
                 (lambda (point)
                   (cdr (search-symbol (line-string point) name)))
                 (lambda (point)
                   (line-offset point 1))
                 (lambda (point charpos)
                   (line-offset point 0 charpos))
                 (search-forward-endp-function limit-point))))

(defun search-backward-symbol (point name &optional limit-point)
  (search-step point
               (lambda (point)
                 (car (search-symbol (line-string point)
                                     name
                                     :end (point-charpos point)
                                     :from-end t)))
               (lambda (point)
                 (car (search-symbol (line-string point)
                                     name
                                     :from-end t)))
               (lambda (point)
                 (line-offset point -1))
               (lambda (point charpos)
                 (line-offset point 0 charpos))
               (search-backward-endp-function limit-point)))

(defun looking-at (point regex)
  (let ((start (point-charpos point))
        (string (line-string point)))
    (multiple-value-bind (match-start match-end reg-starts reg-ends)
        (ppcre:scan regex string :start start)
      (when (eql match-start start)
        (values (subseq string match-start match-end)
                (map 'vector
                     (lambda (reg-start reg-end)
                       (when reg-start
                         (subseq string reg-start reg-end)))
                     reg-starts
                     reg-ends))))))

(defun match-string-at (point string &optional across-line-p)
  (let ((overp
         (> (+ (point-charpos point) (length string))
            (length (line-string point)))))
    (cond ((and across-line-p overp)
           (string= string
                    (points-to-string point
                                      (character-offset (copy-point point :temporary)
                                                        (length string)))))
          (overp
           nil)
          (t
           (string= (line-string point) string
                    :start1 (point-charpos point)
                    :end1 (+ (length string)
                             (point-charpos point)))))))
