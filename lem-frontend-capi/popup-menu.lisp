(defpackage :lem-capi.popup-menu
  (:add-use-defaults t))
(in-package :lem-capi.popup-menu)

(defvar *non-focus-interface*)
(defvar *items*)
(defvar *lem-process*)

(defmethod lem-if:display-popup-menu ((implementation lem-capi::capi-impl) items
                                      &key action-callback print-function focus-attribute non-focus-attribute)
  (declare (ignore focus-attribute non-focus-attribute))
  (setf *lem-process* mp:*current-process*)
  (setf *items* items)
  (multiple-value-bind (char-width char-height)
      (lem-capi.lem-pane::lem-pane-char-size lem-capi::*lem-pane*)
    (multiple-value-bind (x y)
        (lem::compute-pop-up-window-position (lem:current-window))
      (setf *non-focus-interface*
            (capi:prompt-with-list-non-focus
             items
             :owner lem-capi::*lem-pane*
             :x (* x char-width)
             :y (* y char-height)
             :print-function (or print-function #'princ-to-string)
             :action-callback (lambda (item interface)
                                (declare (ignore interface))
                                (mp:process-interrupt *lem-process* action-callback item))
             :list-updater (lambda () *items*))))))

(defmethod lem-if:popup-menu-update ((implementation lem-capi::capi-impl) items)
  (setf *items* items)
  (capi:non-focus-update *non-focus-interface*))

(defmethod lem-if:popup-menu-quit ((implementation lem-capi::capi-impl))
  (capi:non-focus-terminate *non-focus-interface*))

(defmethod lem-if:popup-menu-down ((implementation lem-capi::capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec :down)))

(defmethod lem-if:popup-menu-up ((implementation lem-capi::capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec :up)))

(defmethod lem-if:popup-menu-first ((implementation lem-capi::capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec :prior)))

(defmethod lem-if:popup-menu-last ((implementation lem-capi::capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec :next)))

(defmethod lem-if:popup-menu-select ((implementation lem-capi::capi-impl))
  (capi:non-focus-maybe-capture-gesture *non-focus-interface* (sys:coerce-to-gesture-spec #\return)))

#|
(defun display-popup-menu (lem-pane items x y)
  (capi:apply-in-pane-process
   lem-pane (lambda (lem-pane items)
              (multiple-value-bind (char-width char-height)
                  (lem-pane-char-size lem-pane)
                (popup-menu lem-pane items (* x char-width) (* (1+ y) char-height))))
   lem-pane
   items))

(defun popup-menu (lem-pane items x y)
  (let ((column 0))
    (dolist (item items)
      (setf column
            (max column (lem:string-width
                         (lem.completion-mode::completion-item-label item)))))
    (capi:prompt-with-list-non-focus
     items
     :print-function (lambda (item)
                       (format nil "~A~vT~A"
                               (lem.completion-mode::completion-item-label item)
                               (+ column 2)
                               (lem.completion-mode::completion-item-detail item)))
     :x x
     :y y
     :action-callback (lambda (item interface)
                        ))))
|#

#|
(setq x (capi:prompt-with-list-non-focus
             '("a" "b" "c")
             :x 10
             :y 10
             :action-callback (lambda (&rest args)
                                (print args))
             :list-updater (lambda (&rest args)
                             (print args)
                             t)
             ))
(capi:non-focus-terminate x)
|#
