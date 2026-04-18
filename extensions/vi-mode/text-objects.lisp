(defpackage :lem-vi-mode/text-objects
  (:use :cl
        :lem)
  (:import-from :lem-vi-mode/core
                :make-range
                :range-beginning
                :range-end
                :text-object-abort)
  (:import-from :lem-vi-mode/visual
                :visual
                :visual-p
                :visual-char-p
                :visual-range
                :vi-visual-char)
  (:import-from :lem-vi-mode/word
                :word-char-type
                :broad-word-char-type)
  (:export :text-object
           :function-text-object
           :surrounded-text-object
           :quoted-text-object
           :a-range-of
           :inner-range-of

           :word-object
           :broad-word-object
           :paren-object
           :bracket-object
           :curly-object
           :angle-bracket-object
           :paragraph-object
           :double-quoted-object
           :single-quoted-object
           :back-quoted-object
           :tag-object
           :vi-operator-surrounding-blanks))
(in-package :lem-vi-mode/text-objects)

(define-editor-variable vi-operator-surrounding-blanks nil)

(defclass text-object () ())

(defgeneric a-range-of (object state count)
  (:method ((object symbol) state count)
    (a-range-of (make-instance object) state count)))

(defgeneric on-object-p (object point))
(defgeneric include-surrounding-blanks (object beg end direction on-object))
(defgeneric slurp-object (object point direction))

(defmethod include-surrounding-blanks (object beg end direction on-object)
  ;; If the first position is on the object,
  (if on-object
      ;; Delete trailing spaces.
      (or (/= 0 (skip-chars-forward end '(#\Space #\Tab)))
          (with-point ((p beg))
            (skip-chars-backward p '(#\Space #\Tab))
            (unless (zerop (point-charpos p))
              (move-point beg p))))
      ;; If it's on *not* the object, delete leading spaces.
      (when (null direction)
        (skip-chars-backward beg '(#\Space #\Tab)))))

(defgeneric inner-range-of (object state count)
  (:method ((object symbol) state count)
    (inner-range-of (make-instance object) state count)))

(defun a-range-with-direction (object count beg end direction)
  (check-type direction (member :forward :backward))
  (dotimes (i count)
    (if (eq direction :backward)
        (skip-chars-backward end '(#\Space #\Tab #\Newline))
        (skip-chars-forward end '(#\Space #\Tab #\Newline)))
    (when (or (and (eq direction :backward)
                   (or (start-buffer-p end)
                       (char= (character-at end -1) #\Newline)))
              (and (eq direction :forward)
                   (or (end-buffer-p end)
                       (char= (character-at end) #\Newline))))
      (error 'text-object-abort
             :range (make-range beg end)))
    (slurp-object object end direction))
  (make-range beg end))

(defmethod a-range-of (object (state visual) count)
  (destructuring-bind (beg end)
      (visual-range)
    (when (point= beg (current-point))
      (return-from a-range-of
        (call-next-method)))
    (let ((direction (cond
                       ((point< beg end) :forward)
                       ((point< end beg) :backward)))
          (on-object (on-object-p object end)))
      (ecase direction
        (:forward
         (unless on-object
           (skip-chars-forward end '(#\Space #\Tab #\Newline))
           (character-offset end 1)))
        (:backward
         (unless on-object
           (skip-chars-backward end '(#\Space #\Tab #\Newline))
           (character-offset end -1))))
      (prog1
          (a-range-with-direction object count beg end (or direction :forward))
        (include-surrounding-blanks object beg end direction on-object)))))

(defmethod a-range-of (object state count)
  (declare (ignore state))
  (with-point ((beg (current-point))
               (end (current-point)))
    (let ((on-object (on-object-p object end)))
      (if on-object
          (slurp-object object beg :backward)
          (progn
            (skip-chars-forward end '(#\Space #\Tab))
            (if (char= (character-at end) #\Newline)
                (progn
                  (character-offset end 1)
                  (skip-chars-forward end '(#\Space #\Tab #\Newline)))
                (move-point beg end))
            (character-offset end 1)))
      (prog1
          (a-range-with-direction object count beg end :forward)
        (include-surrounding-blanks object beg end nil on-object)))))

(defmethod inner-range-of (object state count)
  (declare (ignore state))
  (with-point ((beg (current-point))
               (end (current-point)))
    (if (on-object-p object beg)
        (slurp-object object beg :backward)
        (skip-chars-backward beg '(#\Space #\Tab)))
    (dotimes (i count)
      (when (or (end-buffer-p end)
                (char= (character-at end) #\Newline))
        (error 'text-object-abort
               :range (make-range beg end)))
      (if (on-object-p object end)
          (slurp-object object end :forward)
          (skip-chars-forward end '(#\Space #\Tab))))
    (make-range beg end)))

(defmethod inner-range-of (object (state visual) count)
  (destructuring-bind (beg end)
      (visual-range)
    (when (point= beg (current-point))
      (return-from inner-range-of (call-next-method)))

    (let ((direction (cond
                       ((point< beg end) :forward)
                       ((point< end beg) :backward))))
      (ecase direction
        (:forward
         (dotimes (i count)
           (when (or (end-buffer-p end)
                     (char= (character-at end) #\Newline))
             (error 'text-object-abort
                    :range (make-range beg end)))
           (slurp-object object end :forward)))
        (:backward
         (slurp-object object beg :forward)
         (dotimes (i count)
           (when (or (start-buffer-p end)
                     (char= (character-at end -1) #\Newline))
             (error 'text-object-abort
                    :range (make-range beg end)))
           (slurp-object object end :backward)))))
    (make-range beg end)))

;;
;; function-text-object

(defclass function-text-object (text-object)
  ((function :type function
             :initarg :function)))

(defmethod on-object-p ((object function-text-object) point)
  (not (member (character-at point) '(#\Space #\Tab #\Newline))))

(defmethod slurp-object ((object function-text-object) point direction)
  (check-type direction (member :forward :backward))
  (with-slots (function) object
    (let* ((char-type (funcall function (character-at point)))
           (check-fn (lambda (c) (eql char-type (funcall function c))))
           (buffer (point-buffer point)))
      (labels ((move-forward (p)
                 (loop with buffer-end = (buffer-end-point buffer)
                       if (or (point= p buffer-end)
                              (char= (character-at p) #\Newline))
                       do (return nil)
                       else if (funcall check-fn (character-at p))
                       do (character-offset p 1)
                       else
                       do (return t)))
               (move-backward (p)
                 (loop while (and (< 0 (point-charpos p))
                                  (funcall check-fn (character-at p -1)))
                       do (character-offset p -1))
                 p))
        (if (eq direction :forward)
            (move-forward point)
            (move-backward point)))))
  point)

;;
;; block-text-object

(defclass block-text-object (text-object)
  ((open-char :type character
              :initarg :open-char)
   (close-char :type character
               :initarg :close-char)))

(defmethod initialize-instance :after ((object block-text-object) &key)
  "Derive close-char from open-char if not explicitly provided."
  (unless (slot-boundp object 'close-char)
    (with-slots (open-char close-char) object
      (setf close-char
            (ecase open-char
              (#\( #\))
              (#\[ #\])
              (#\{ #\}))))))

(defun %find-enclosing-block (point open-char close-char)
  "Find the block pair enclosing POINT using character-level matching.
Returns (values open-point past-close-point) or NIL."
  (with-point ((open point))
    ;; Search backward for unmatched open-char
    (let ((depth 0))
      (loop :do
        (let ((c (character-at open)))
          (cond
            ((null c)
             (return-from %find-enclosing-block nil))
            ((char= c close-char) (incf depth))
            ((char= c open-char)
             (if (zerop depth) (return) (decf depth)))))
        (if (start-buffer-p open)
            (return-from %find-enclosing-block nil)
            (character-offset open -1))))
    ;; open is now at the open-char. Search forward for matching close-char.
    (with-point ((close open))
      (character-offset close 1)
      (let ((depth 0))
        (loop :do
          (let ((c (character-at close)))
            (cond
              ((null c)
               (return-from %find-enclosing-block nil))
              ((char= c open-char) (incf depth))
              ((char= c close-char)
               (if (zerop depth)
                   (progn
                     (character-offset close 1)
                     (return-from %find-enclosing-block
                       (values open close)))
                   (decf depth)))))
          (character-offset close 1))))))

(defmethod a-range-of ((object block-text-object) state count)
  (with-slots (open-char close-char) object
    (with-point ((p (current-point)))
      (dotimes (i count)
        (multiple-value-bind (open close)
            (%find-enclosing-block p open-char close-char)
          (declare (ignore close))
          (unless open (error 'text-object-abort))
          (when (> i 0)
            (move-point p open)
            (character-offset p -1))))
      (multiple-value-bind (open close)
          (%find-enclosing-block p open-char close-char)
        (unless open (error 'text-object-abort))
        (make-range open close)))))

(defmethod inner-range-of ((object block-text-object) state count)
  (let ((range (a-range-of object state count)))
    (character-offset (range-beginning range) 1)
    (character-offset (range-end range) -1)
    range))

(defmethod a-range-of :before ((object block-text-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

(defmethod inner-range-of :before ((object block-text-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

;;
;; quoted-text-object

(defclass quoted-text-object (text-object)
  ((quote-char :type character
               :initarg :quote-char)
   (escape-char :type (or null character)
                :initarg :escape-char
                :initform #\\)))

(defun %count-quote-between (point1 point2 &key quote-char escape-char)
  (check-type quote-char character)
  (check-type escape-char (or null character))
  (when (point= point1 point2)
    (return-from %count-quote-between 0))
  (when (point< point2 point1)
    (rotatef point1 point2))
  (let* ((string (points-to-string point1 point2))
         (limit (length string))
         (quote-count 0))
    (do ((i 0 (1+ i)))
        ((<= limit i) quote-count)
      (let ((char (aref string i)))
        (cond
          ((and escape-char
                (char= char escape-char))
           (incf i))
          ((char= char quote-char)
           (incf quote-count)))))))

(defmethod on-object-p ((object quoted-text-object) point)
  (with-slots (quote-char escape-char) object
    (with-point ((bol point))
      (line-start bol)
      (or (= (mod (%count-quote-between bol point
                                     :quote-char quote-char
                                     :escape-char escape-char)
               2) 1)
          (and (char= (character-at point) quote-char)
               (or (null escape-char)
                   (char= (character-at point -1) escape-char)))))))

(defmethod slurp-object ((object quoted-text-object) point direction)
  (with-slots (quote-char escape-char) object
    (ecase direction
      (:backward
       (loop
         (skip-chars-backward point (lambda (c) (char/= c quote-char)))
         (let ((prev-char (character-at point -1)))
           (cond
             ;; No quote-char found
             ((null prev-char)
              (keyboard-quit))

             ;; Skip escape & quote-char
             ((and escape-char
                   (character-at point -2) ;; Bound check
                   (char= (character-at point -2) escape-char))
              (character-offset point -2))

             ;; Successfully found unescaped quote
             (t
              (character-offset point -1)
              (return))))))
      (:forward
       (loop
         (skip-chars-forward point (lambda (c) (char/= c quote-char)))
         (let ((next-char (character-at point)))
           (cond
             ;; No quote-char found
             ((null next-char)
              (keyboard-quit))

             ;; Skip escape & quote-char
             ((and escape-char
                   (character-at point 2) ;; Bound Check
                   (char= (character-at point -1) escape-char))
              (character-offset point 2))

             ;; Successfully found
             (t
              (character-offset point 1)
              (return)))))))))

(defmethod inner-range-of ((object quoted-text-object) state count)
  (declare (ignore state count))
  (let ((range (call-next-method)))
    (character-offset (range-beginning range) 1)
    (character-offset (range-end range) -1)
    range))

(defmethod include-surrounding-blanks ((object quoted-text-object) beg end direction on-object)
  (when (variable-value 'vi-operator-surrounding-blanks)
  ;; Trailing first
    (or (/= 0 (if (eq direction :backward)
                (skip-chars-backward end '(#\Space #\Tab))
                (skip-chars-forward end '(#\Space #\Tab))))
      ;; If no trailing spaces, delete leading spaces unless it's the beginning indentation
      (with-point ((p beg))
        (if (eq direction :backward)
            (progn
              (skip-chars-forward p '(#\Space #\Tab))
              (unless (or (end-buffer-p p)
                          (char= (character-at p) #\Newline))
                (move-point beg p)))
            (progn
              (skip-chars-backward p '(#\Space #\Tab))
              (unless (zerop (point-charpos p))
                (move-point beg p))))))))

;;
;; word-object

(defclass word-object (function-text-object) ()
  (:default-initargs
   :function #'word-char-type))

(defmethod a-range-of :before ((object word-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

(defmethod inner-range-of :before ((object word-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

;;
;; broad-word-object

(defclass broad-word-object (function-text-object) ()
  (:default-initargs
   :function #'broad-word-char-type))

(defmethod a-range-of :before ((object broad-word-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

(defmethod inner-range-of :before ((object broad-word-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

;;
;; paren-object

(defclass paren-object (block-text-object) ()
  (:default-initargs
   :open-char #\())

;;
;; double-quoted-object

(defclass double-quoted-object (quoted-text-object) ()
  (:default-initargs
   :quote-char #\"))

;;
;; single-quoted-object

(defclass single-quoted-object (quoted-text-object) ()
  (:default-initargs
   :quote-char #\'
   :escape-char nil)
  (:documentation "Text object for single-quoted strings ('...')."))

;;
;; back-quoted-object

(defclass back-quoted-object (quoted-text-object) ()
  (:default-initargs
   :quote-char #\`
   :escape-char nil)
  (:documentation "Text object for back-quoted strings (`...`)."))

;;
;; bracket-object

(defclass bracket-object (block-text-object) ()
  (:default-initargs
   :open-char #\[)
  (:documentation "Text object for square bracket pairs ([...])."))

;;
;; curly-object

(defclass curly-object (block-text-object) ()
  (:default-initargs
   :open-char #\{)
  (:documentation "Text object for curly brace pairs ({...})."))

;;
;; angle-bracket-object

(defclass angle-bracket-object (text-object) ()
  (:documentation "Text object for angle bracket pairs (<...>)."))

(defun %find-enclosing-angle-brackets (point)
  "Find the angle bracket pair enclosing POINT.
Returns (values open-point past-close-point) or NIL."
  (with-point ((open point))
    ;; Search backward (including current position) for unmatched <
    (let ((depth 0))
      (loop :do
        (let ((c (character-at open)))
          (cond
            ((null c)
             (return-from %find-enclosing-angle-brackets nil))
            ((char= c #\>) (incf depth))
            ((char= c #\<)
             (if (zerop depth) (return) (decf depth)))))
        (if (start-buffer-p open)
            (return-from %find-enclosing-angle-brackets nil)
            (character-offset open -1))))
    ;; open is now at the <. Search forward for matching >
    (with-point ((close open))
      (character-offset close 1)
      (let ((depth 0))
        (loop :do
          (let ((c (character-at close)))
            (cond
              ((null c)
               (return-from %find-enclosing-angle-brackets nil))
              ((char= c #\<) (incf depth))
              ((char= c #\>)
               (if (zerop depth)
                   (progn
                     (character-offset close 1)
                     (return-from %find-enclosing-angle-brackets
                       (values open close)))
                   (decf depth)))))
          (character-offset close 1))))))

(defmethod a-range-of ((object angle-bracket-object) state count)
  (declare (ignore state))
  (with-point ((p (current-point)))
    (dotimes (i count)
      (multiple-value-bind (open close)
          (%find-enclosing-angle-brackets p)
        (declare (ignore close))
        (unless open (error 'text-object-abort))
        (when (> i 0)
          (move-point p open)
          (character-offset p -1))))
    (multiple-value-bind (open close)
        (%find-enclosing-angle-brackets p)
      (unless open (error 'text-object-abort))
      (make-range open close))))

(defmethod inner-range-of ((object angle-bracket-object) state count)
  (let ((range (a-range-of object state count)))
    (character-offset (range-beginning range) 1)
    (character-offset (range-end range) -1)
    range))

(defmethod a-range-of :before ((object angle-bracket-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

(defmethod inner-range-of :before ((object angle-bracket-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

;;
;; tag-object

(defclass tag-object (text-object) ()
  (:documentation "Text object for HTML/XML tag pairs (<tag>...</tag>)."))

(defun %find-tag-backward (point)
  "Search backward from POINT to find the start of an opening tag.
Returns the point positioned at the '<' of the opening tag, or NIL."
  (with-point ((p point))
    (loop :do
      (unless (search-backward p "<")
        (return nil))
      ;; Skip closing tags
      (when (and (character-at p 1)
                 (char= (character-at p 1) #\/))
        (character-offset p -1)
        (when (start-buffer-p p)
          (return nil)))
      ;; Found an opening tag (not a closing tag)
      (when (or (null (character-at p 1))
                (char/= (character-at p 1) #\/))
        (return p)))))

(defun %find-matching-close-tag (point)
  "From POINT at '<' of an opening tag, find the matching closing tag.
Returns a range from the start of the opening tag to the end of the closing tag."
  (with-point ((start point)
               (p point))
    ;; Extract the tag name
    (character-offset p 1)
    (let ((tag-start (copy-point p :temporary)))
      (skip-chars-forward p (lambda (c)
                              (and (char/= c #\Space)
                                   (char/= c #\>)
                                   (char/= c #\/)
                                   (char/= c #\Newline))))
      (let ((tag-name (points-to-string tag-start p)))
        (when (string= tag-name "")
          (return-from %find-matching-close-tag nil))
        ;; Find the end of the opening tag
        (unless (search-forward p ">")
          (return-from %find-matching-close-tag nil))
        ;; Check for self-closing tag
        (when (char= (character-at p -2) #\/)
          (return-from %find-matching-close-tag nil))
        (with-point ((content-start p))
          ;; Now find the matching closing tag, handling nesting
          (let ((depth 1)
                (open-pattern (format nil "<~A" tag-name))
                (close-pattern (format nil "</~A" tag-name)))
            (loop :do
              (with-point ((next-open p)
                           (next-close p))
                (let ((found-open (search-forward next-open open-pattern))
                      (found-close (search-forward next-close close-pattern)))
                  (cond
                    ((not found-close)
                     (return nil))
                    ((and found-open (point< found-open found-close))
                     ;; Check it's actually an opening tag (not just a prefix match)
                     (let ((after-name (character-at found-open)))
                       (when (or (null after-name)
                                 (char= after-name #\Space)
                                 (char= after-name #\>)
                                 (char= after-name #\/)
                                 (char= after-name #\Newline))
                         (incf depth)))
                     (move-point p found-open))
                    (t
                     (decf depth)
                     (if (zerop depth)
                         (progn
                           ;; Compute inner-end: start of closing tag
                           (with-point ((content-end found-close))
                             (character-offset content-end (- (length close-pattern)))
                             ;; Move past closing tag
                             (move-point p found-close)
                             (search-forward p ">")
                             (return (values start p content-start content-end))))
                         (move-point p found-close)))))))))))))

(defun %find-tag-around-point (point)
  "Find the innermost tag pair surrounding POINT.
Returns four values: outer-start, outer-end, inner-start, inner-end."
  (with-point ((search-point point))
    ;; Search backward for opening tags and check if they enclose point
    (loop :do
      (let ((tag-start (%find-tag-backward search-point)))
        (unless tag-start
          (return nil))
        (multiple-value-bind (outer-start outer-end inner-start inner-end)
            (%find-matching-close-tag tag-start)
          (when (and outer-start
                     (point<= outer-start point)
                     (point<= point outer-end))
            (return (values outer-start outer-end inner-start inner-end))))
        ;; This tag didn't enclose point, keep searching backward
        (move-point search-point tag-start)
        (when (start-buffer-p search-point)
          (return nil))
        (character-offset search-point -1)))))

(defmethod a-range-of ((object tag-object) state count)
  (declare (ignore state))
  (with-point ((p (current-point)))
    (dotimes (i count)
      (multiple-value-bind (outer-start outer-end)
          (%find-tag-around-point p)
        (unless outer-start
          (error 'text-object-abort))
        (when (> i 0)
          (move-point p outer-start)
          (character-offset p -1))))
    (multiple-value-bind (outer-start outer-end)
        (%find-tag-around-point p)
      (unless outer-start
        (error 'text-object-abort))
      (make-range outer-start outer-end))))

(defmethod inner-range-of ((object tag-object) state count)
  (declare (ignore state))
  (with-point ((p (current-point)))
    (dotimes (i count)
      (multiple-value-bind (outer-start outer-end inner-start inner-end)
          (%find-tag-around-point p)
        (declare (ignore outer-end))
        (unless outer-start
          (error 'text-object-abort))
        (when (> i 0)
          (move-point p outer-start)
          (character-offset p -1))))
    (multiple-value-bind (outer-start outer-end inner-start inner-end)
        (%find-tag-around-point p)
      (declare (ignore outer-start outer-end))
      (unless inner-start
        (error 'text-object-abort))
      (make-range inner-start inner-end))))

(defmethod a-range-of :before ((object tag-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

(defmethod inner-range-of :before ((object tag-object) (state visual) count)
  (unless (visual-char-p)
    (vi-visual-char)))

;;
;; paragraph-object
(defclass paragraph-object (text-object) ())

(defmethod inner-range-of ((object paragraph-object) state count)
  (declare (ignore state count))
  (with-point ((start (current-point))
               (end (current-point)))
    ;; Start
    (loop until (or (start-buffer-p start)
                    (blank-line-p start))
          do (line-offset start -1))
    (when (blank-line-p start) ;; pull back into paragraph
      (line-offset start 1))

    ;; End
    (loop until (or (end-buffer-p end)
                    (blank-line-p end))
          do (line-offset end 1))
    (make-range start end)))

;; adds on additional blank lines for inner paragraph-object
(defmethod a-range-of ((object paragraph-object) state count)
  (let ((range (inner-range-of 'paragraph-object state count)))
    (line-offset (range-end range) 1)
    (loop until (or (end-buffer-p (range-end range))
                    (not (blank-line-p (range-end range))))
          do (line-offset (range-end range) 1))
    range))
