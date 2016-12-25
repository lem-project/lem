(in-package :lem)

(export '(*case-fold-search*
          search-forward
          search-backward
          search-forward-regexp
          search-backward-regexp
          search-forward-symbol
          search-backward-symbol
          looking-at-line))

(defvar *case-fold-search* nil)

(defun search-step (marker first-search search step move-matched endp)
  (with-marker ((start-marker marker))
    (let ((result
           (let ((res (funcall first-search marker)))
             (cond (res
                    (funcall move-matched marker res)
                    (not (funcall endp marker)))
                   (t
                    (loop :until (funcall endp marker) :do
                          (unless (funcall step marker)
                            (return nil))
                          (let ((res (funcall search marker)))
                            (when res
                              (funcall move-matched marker res)
                              (return t)))))))))
      (unless result
        (move-point marker start-marker))
      result)))

(defun search-forward-endp-function (limit-marker)
  (if limit-marker
      (lambda (marker)
        (or (point<= limit-marker marker)
            (end-buffer-p marker)))
      #'end-buffer-p))

(defun search-backward-endp-function (limit-marker)
  (if limit-marker
      (lambda (marker)
        (point< marker limit-marker))
      #'start-buffer-p))

(defmacro search* (&rest args)
  `(search ,@args
           :test (if *case-fold-search*
                     #'char=
                     #'char-equal)))

(defun search-forward (marker string &optional limit-marker)
  (let ((nlines (count #\newline string)))
    (flet ((take-string (marker)
             (with-marker ((start-marker marker)
                           (end-marker marker))
               (points-to-string (line-start start-marker)
                                 (line-end (or (line-offset end-marker nlines)
                                               (buffer-end end-marker)))))))
      (search-step marker
                   (lambda (marker)
                     (search* string
                              (take-string marker)
                              :start2 (point-charpos marker)))
                   (lambda (marker)
                     (search* string (take-string marker)))
                   (lambda (marker)
                     (line-offset marker 1))
                   (lambda (marker charpos)
                     (character-offset (line-start marker)
                                       (+ charpos (length string))))
                   (search-forward-endp-function limit-marker)))))

(defun search-backward (marker string &optional limit-marker)
  (let ((nlines (count #\newline string)))
    (flet ((search-from-end (marker end-charpos)
             (with-marker ((marker marker))
               (when (line-offset marker (- nlines))
                 (search* string
                          (points-to-string
                           (line-start (copy-point marker :temporary))
                           (with-marker ((point marker))
                             (unless (line-offset point nlines)
                               (buffer-end point))
                             (if end-charpos
                                 (character-offset point end-charpos)
                                 (line-end point))))
                          :from-end t)))))
      (let ((end-charpos (point-charpos marker)))
        (search-step marker
                     (lambda (marker)
                       (search-from-end marker end-charpos))
                     (lambda (marker)
                       (search-from-end marker nil))
                     (lambda (marker)
                       (line-offset marker -1))
                     (lambda (marker charpos)
                       (unless (zerop nlines)
                         (line-offset marker (- nlines)))
                       (character-offset (line-start marker) charpos))
                     (search-backward-endp-function limit-marker))))))

(defun search-forward-regexp (marker regex &optional limit-marker)
  (let ((scanner (ignore-errors (ppcre:create-scanner regex))))
    (when scanner
      (search-step marker
                   (lambda (marker)
                     (multiple-value-bind (start end)
                         (ppcre:scan scanner
                                     (line-string-at marker)
                                     :start (point-charpos marker))
                       (when (and start
                                  (if (= start end)
                                      (< (point-charpos marker) start)
                                      (<= (point-charpos marker) start)))
                         (if (= start end)
                             (1+ end)
                             end))))
                   (lambda (marker)
                     (nth-value 1
                                (ppcre:scan scanner
                                            (line-string-at marker))))
                   (lambda (marker)
                     (line-offset marker 1))
                   (lambda (marker charpos)
                     (character-offset (line-start marker) charpos))
                   (search-forward-endp-function limit-marker)))))

(defun search-backward-regexp (marker regex &optional limit-marker)
  (let ((scanner (ignore-errors (ppcre:create-scanner regex))))
    (when scanner
      (search-step marker
                   (lambda (marker)
                     (let (pos)
                       (ppcre:do-scans (start end reg-starts reg-ends scanner
                                              (line-string-at marker) nil
                                              :end (point-charpos marker))
                         (setf pos start))
                       pos))
                   (lambda (marker)
                     (let (pos)
                       (ppcre:do-scans (start end reg-starts reg-ends scanner
                                              (line-string-at marker) nil
                                              :start (point-charpos marker))
                         (setf pos start))
                       pos))
                   (lambda (marker)
                     (line-offset marker -1))
                   (lambda (marker charpos)
                     (character-offset (line-start marker) charpos))
                   (search-backward-endp-function limit-marker)))))

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
              (setf start (1+ (or pos start))))))

(defun search-forward-symbol (marker name &optional limit-marker)
  (let ((charpos (point-charpos marker)))
    (search-step marker
                 (lambda (marker)
                   (cdr (search-symbol (line-string-at marker) name :start charpos)))
                 (lambda (marker)
                   (cdr (search-symbol (line-string-at marker) name)))
                 (lambda (marker)
                   (line-offset marker 1))
                 (lambda (marker charpos)
                   (line-offset marker 0 charpos))
                 (search-forward-endp-function limit-marker))))

(defun search-backward-symbol (marker name &optional limit-marker)
  (search-step marker
               (lambda (marker)
                 (car (search-symbol (line-string-at marker)
                                     name
                                     :end (point-charpos marker)
                                     :from-end t)))
               (lambda (marker)
                 (car (search-symbol (line-string-at marker)
                                     name
                                     :from-end t)))
               (lambda (marker)
                 (line-offset marker -1))
               (lambda (marker charpos)
                 (line-offset marker 0 charpos))
               (search-backward-endp-function limit-marker)))

(defun looking-at-line (regex &key (start nil startp) (end nil endp))
  (macrolet ((m (&rest args)
                `(ppcre:scan-to-strings regex
                                        (line-string-at (current-point))
                                        ,@args)))
    (cond ((and startp endp)
           (m :start start :end end))
          (startp
           (m :start start))
          (endp
           (m :end end))
          (t
           (m)))))
