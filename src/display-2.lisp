(defpackage :lem-core/display-2
  (:use :cl))
(in-package :lem-core/display-2)

(defun make-cursor-overlay (point)
  (let ((overlay
          (lem-core:make-overlay point
                                 (lem-core:with-point ((p point))
                                   (lem-core:character-offset p 1)
                                   p)
                                 (if (typep point 'lem-core:fake-cursor)
                                     'lem-core:fake-cursor
                                     'lem-core:cursor)
                                 :temporary t)))
    (lem-core:overlay-put overlay :cursor t)
    overlay))

(defun collect-overlays (window)
  (let ((overlays (lem-core::get-window-overlays window)))
    (if (and (eq window (lem-core:current-window))
             (not (lem-core:window-cursor-invisible-p window)))
        (append overlays
                (mapcar #'make-cursor-overlay
                        (lem-core:buffer-cursors (lem-core:window-buffer window))))
        overlays)))

(defstruct logical-line
  string
  attributes
  left-content
  end-of-line-cursor-attribute
  extend-to-end
  line-end-overlay)

(defun attribute-equal-careful-null-and-symbol (a b)
  (if (or (null a) (null b))
      (and (null a) (null b))
      (lem-core::attribute-equal (lem-core:ensure-attribute a)
                                 (lem-core:ensure-attribute b))))

(defun logical-line-equal (a b)
  (and (string= (logical-line-string a) (logical-line-string b))
       (= (length (logical-line-attributes a))
          (length (logical-line-attributes b)))
       (every (lambda (elt1 elt2)
                (and (equal (first elt1) (first elt2))
                     (equal (second elt1) (second elt2))
                     (attribute-equal-careful-null-and-symbol (third elt1) (third elt2))))
              (logical-line-attributes a)
              (logical-line-attributes b))
       (attribute-equal-careful-null-and-symbol (logical-line-end-of-line-cursor-attribute a)
                                                (logical-line-end-of-line-cursor-attribute b))
       (attribute-equal-careful-null-and-symbol (logical-line-extend-to-end a)
                                                (logical-line-extend-to-end b))))

(defun overlay-within-point-p (overlay point)
  (or (lem-core:point<= (lem-core:overlay-start overlay)
                        point
                        (lem-core:overlay-end overlay))
      (lem-core:same-line-p (lem-core:overlay-start overlay)
                            point)
      (lem-core:same-line-p (lem-core:overlay-end overlay)
                            point)))

(defun overlay-cursor-p (overlay)
  (lem-core:overlay-get overlay :cursor))

(defun cursor-attribute-p (attribute)
  (and (lem-core:attribute-p attribute)
       (lem-core:attribute-value attribute :cursor)))

(defun set-cursor-attribute (attribute)
  (setf (lem-core:attribute-value attribute :cursor) t))

(defun overlay-start-charpos (overlay point)
  (if (lem-core:same-line-p point (lem-core:overlay-start overlay))
      (lem-core:point-charpos (lem-core:overlay-start overlay))
      0))

(defun overlay-end-charpos (overlay point)
  (cond ((and (overlay-cursor-p overlay)
              (lem-core:point= (lem-core:overlay-start overlay) (lem-core:overlay-end overlay)))
         ;; cursor is end-of-buffer
         nil)
        ((lem-core:same-line-p point (lem-core:overlay-end overlay))
         (lem-core:point-charpos (lem-core:overlay-end overlay)))
        (t
         nil)))

(defun expand-tab (string attributes tab-width)
  (setf attributes (copy-tree attributes))
  (values (with-output-to-string (out)
            (loop :with i := 0
                  :for c :across string
                  :do (cond ((char= c #\tab)
                             (let ((n (- tab-width (mod i tab-width))))
                               (loop :for elt :in attributes
                                     :do (cond ((< i (first elt))
                                                (incf (first elt) (1- n))
                                                (incf (second elt) (1- n)))
                                               ((and (< i (second elt))
                                                     (not (cursor-attribute-p (third elt))))
                                                (incf (second elt) (1- n)))))
                               (loop :repeat n
                                     :do (write-char #\space out))
                               (incf i n)))
                            (t
                             (write-char c out)
                             (incf i)))))
          attributes))

(defun create-logical-line (point overlays active-modes)
  (let* ((end-of-line-cursor-attribute nil)
         (extend-to-end-attribute nil)
         (line-end-overlay nil)
         (left-content
           (lem-core::compute-left-display-area-content active-modes
                                                        (lem-base:point-buffer point)
                                                        point))
         (tab-width (lem-core:variable-value 'lem-core:tab-width :default point)))
    (destructuring-bind (string . attributes)
        (lem-base::line-string/attributes (lem-base::point-line point))
      (loop :for overlay :in overlays
            :when (overlay-within-point-p overlay point)
            :do (cond ((typep overlay 'lem-core::overlay-line-endings)
                       (setf line-end-overlay overlay))
                      ((typep overlay 'lem-core::overlay-line)
                       (let ((attribute (lem-core:overlay-attribute overlay)))
                         (setf attributes
                               (lem-core::overlay-attributes attributes
                                                             0
                                                             (length string)
                                                             attribute))
                         (setf extend-to-end-attribute attribute)))
                      (t
                       (let ((overlay-start-charpos (overlay-start-charpos overlay point))
                             (overlay-end-charpos (overlay-end-charpos overlay point))
                             (overlay-attribute (lem-core:overlay-attribute overlay)))
                         (cond ((overlay-cursor-p overlay)
                                (set-cursor-attribute overlay-attribute)
                                (unless overlay-end-charpos
                                  (setf end-of-line-cursor-attribute overlay-attribute)))
                               ((null overlay-end-charpos)
                                (setf extend-to-end-attribute
                                      (lem-core:overlay-attribute overlay))))
                         (setf attributes
                               (lem-core::overlay-attributes
                                attributes
                                overlay-start-charpos
                                (or overlay-end-charpos (length string))
                                overlay-attribute))))))
      (setf (values string attributes) (expand-tab string attributes tab-width))
      (make-logical-line :string string
                         :attributes attributes
                         :left-content left-content
                         :extend-to-end extend-to-end-attribute
                         :end-of-line-cursor-attribute end-of-line-cursor-attribute
                         :line-end-overlay line-end-overlay))))


(defstruct string-with-attribute-item
  string
  attribute)

(defstruct cursor-item
  attribute
  string)

(defstruct eol-cursor-item
  attribute)

(defstruct extend-to-eol-item
  color)

(defstruct line-end-item
  text
  attribute
  offset)

(defmethod item-string ((item string-with-attribute-item))
  (string-with-attribute-item-string item))

(defmethod item-string ((item cursor-item))
  (cursor-item-string item))

(defmethod item-string ((item eol-cursor-item))
  " ")

(defmethod item-string ((item extend-to-eol-item))
  "")

(defmethod item-attribute ((item string-with-attribute-item))
  (string-with-attribute-item-attribute item))

(defmethod item-attribute ((item cursor-item))
  (cursor-item-attribute item))

(defmethod item-attribute ((item eol-cursor-item))
  (eol-cursor-item-attribute item))

(defmethod item-attribute ((item extend-to-eol-item))
  nil)

(defun compute-items-from-string-and-attributes (string attributes)
  (handler-case
      (let ((items '()))
        (flet ((add (item)
                 (if (null items)
                     (push item items)
                     (let ((last-item (first items)))
                       (if (and (string-with-attribute-item-p last-item)
                                (string-with-attribute-item-p item)
                                (equal (string-with-attribute-item-attribute last-item)
                                       (string-with-attribute-item-attribute item)))
                           (setf (string-with-attribute-item-string (first items))
                                 (str:concat (string-with-attribute-item-string last-item)
                                             (string-with-attribute-item-string item)))
                           (push item items))))))
          (loop :for last-pos := 0 :then end
                :for (start end attribute) :in attributes
                :do (unless (= last-pos start)
                      (add (make-string-with-attribute-item :string (subseq string last-pos start))))
                    (add (if (cursor-attribute-p attribute)
                             (make-cursor-item :string (subseq string start end) :attribute attribute)
                             (make-string-with-attribute-item
                              :string (subseq string start end)
                              :attribute attribute)))
                :finally (push (make-string-with-attribute-item :string (subseq string last-pos))
                               items)))
        items)
    (error (e)
      (log:error e string attributes)
      nil)))

(defun attribute-foreground-color (attribute)
  (assert attribute)
  (lem-core:parse-color (lem-core:attribute-foreground attribute)))

(defun attribute-background-color (attribute)
  (assert attribute)
  (lem-core:parse-color (lem-core:attribute-background attribute)))

(defun compute-items-from-logical-line (logical-line)
  (let ((items
          (compute-items-from-string-and-attributes (logical-line-string logical-line)
                                                    (logical-line-attributes logical-line))))
    (alexandria:when-let (attribute
                          (logical-line-extend-to-end logical-line))
      (push (make-extend-to-eol-item :color (attribute-background-color attribute))
            items))
    (alexandria:when-let (attribute
                          (logical-line-end-of-line-cursor-attribute logical-line))
      (push (make-eol-cursor-item :attribute attribute)
            items))
    (values (nreverse items)
            (alexandria:when-let (overlay
                                  (logical-line-line-end-overlay logical-line))
              (make-line-end-item :text (lem-core::overlay-line-endings-text overlay)
                                  :attribute (lem-core:overlay-attribute overlay)
                                  :offset (lem-core::overlay-line-endings-offset overlay))))))

(defun call-do-logical-line (window function)
  (lem-core:with-point ((point (lem-core:window-view-point window)))
    (let ((overlays (collect-overlays window))
          (active-modes (lem-core::get-active-modes-class-instance (lem-core:window-buffer window))))
      (loop :for logical-line := (create-logical-line point overlays active-modes)
            :do (funcall function logical-line)
                (unless (lem-core:line-offset point 1)
                  (return))))))

(defmacro do-logical-line ((logical-line window) &body body)
  `(call-do-logical-line ,window (lambda (,logical-line) ,@body)))
