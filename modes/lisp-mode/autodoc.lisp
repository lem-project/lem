(defpackage :lem-lisp-mode.autodoc
  (:use :cl :lem :lem-lisp-mode))
(in-package :lem-lisp-mode.autodoc)

(define-key *lisp-mode-keymap* "Space" 'lisp-insert-space-and-autodoc)
(define-key *lisp-mode-keymap* "C-c C-d C-a" 'lisp-autodoc)


(let ((autodoc-symbol nil))
  (defun autodoc-symbol ()
    (or autodoc-symbol
        (setf autodoc-symbol (intern "AUTODOC" :swank)))))

(defun autodoc (function)
  (let ((context (lem-lisp-syntax:parse-for-swank-autodoc (current-point))))
    (lisp-eval-async
     `(,(autodoc-symbol) ',context)
     (lambda (doc)
       (trivia:match doc
         ((list doc _)
          (unless (eq doc :not-available)
            (let* ((buffer (make-buffer "*swank:autodoc-fontity*"
                                        :temporary t :enable-undo-p nil))
                   (point (buffer-point buffer)))
              (erase-buffer buffer)
              (change-buffer-mode buffer 'lisp-mode)
              (insert-string point doc)
              (buffer-start point)
              (multiple-value-bind (result string)
                  (search-forward-regexp point "(?====> (.*) <===)")
                (when result
                  (with-point ((start point))
                    (character-offset point 5)
                    (search-forward point "<===")
                    (delete-between-points start point)
                    (insert-string point string :attribute 'region))))
              (buffer-start (buffer-point buffer))
              (setf (variable-value 'truncate-lines :buffer buffer) nil)
              (funcall function buffer)))))))))


(defun lisp-buffer-p (buffer)
  (member (buffer-major-mode buffer)
          '(lisp-mode lisp-repl-mode)))

(defun on-symbol-p (point)
  (or (syntax-symbol-char-p (character-at point))
      (syntax-symbol-char-p (character-at point -1))))

(defun point-on-same-symbol-p (point1 point2)
  (flet ((range (point)
           (with-point ((start point)
                        (end point))
             (skip-chars-backward start #'syntax-symbol-char-p)
             (skip-chars-forward end #'syntax-symbol-char-p)
             (values start end))))
    (and (on-symbol-p point1)
         (on-symbol-p point2)
         (multiple-value-bind (start1 end1) (range point1)
           (multiple-value-bind (start2 end2) (range point2)
             (and start1 end1
                  start2 end2
                  (point= start1 start2)
                  (point= end1 end2)))))))


;;; TODO: isearch-modeやcompletion-modeという具体的なモードを指定しないようにする

(defparameter *disable-autodoc-minor-modes* '(lem.isearch:isearch-mode lem.completion-mode:completion-mode))

(defun conflict-minor-mode-p ()
  (dolist (mode (buffer-minor-modes (current-buffer)) t)
    (when (member mode *disable-autodoc-minor-modes*)
      (return nil))))


(defgeneric should-use-autodoc-p (judgement point))
(defgeneric should-continue-autodoc-p (judgement point))
(defgeneric reset-state (judgement))

(defclass autodoc-judgement ()
  ((last-point
    :initform nil
    :accessor autodoc-judgement-last-point)
   (modified-tick
    :initform nil
    :accessor autodoc-judgement-modified-tick)))

(defvar *judgement-instance* nil)

(defun judgement-instance ()
  (or *judgement-instance*
      (setf *judgement-instance* (make-instance 'autodoc-judgement))))

(defun rotten-last-point (autodoc-judgement)
  (if (null (autodoc-judgement-last-point autodoc-judgement))
      nil
      (not (eql (autodoc-judgement-modified-tick autodoc-judgement)
                (buffer-modified-tick (point-buffer (autodoc-judgement-last-point autodoc-judgement)))))))

(defun care-last-point (autodoc-judgement)
  ;; last-pointはtemporaryなのでバッファ変更後の位置が行の外側を指す場合がある
  ;; そのためlast-point保存時にbuffer-tickも保存しておき、
  ;; 保存時のbuffer-tickと違う(変更されている)場合は無効なものとして扱う
  (when (rotten-last-point autodoc-judgement)
    (setf (autodoc-judgement-last-point autodoc-judgement) nil)
    (setf (autodoc-judgement-modified-tick autodoc-judgement) nil)))

(defmethod should-use-autodoc-p ((judgement autodoc-judgement) point)
  (care-last-point judgement)
  (let ((result
          (and (conflict-minor-mode-p)
               (lisp-buffer-p (point-buffer point))
               (on-symbol-p point)
               (or (null (autodoc-judgement-last-point judgement))
                   (not (point-on-same-symbol-p point (autodoc-judgement-last-point judgement)))))))
    (when result
      (setf (autodoc-judgement-last-point judgement)
            (copy-point point :temporary))
      (setf (autodoc-judgement-modified-tick judgement)
            (buffer-modified-tick (point-buffer point))))
    result))

(defmethod should-continue-autodoc-p ((judgement autodoc-judgement) point)
  (care-last-point judgement)
  (and (lisp-buffer-p (point-buffer point))
       (autodoc-judgement-last-point judgement)
       (eq (point-buffer (autodoc-judgement-last-point judgement))
           (point-buffer point))
       (point-on-same-symbol-p point (autodoc-judgement-last-point judgement))))

(defmethod reset-state ((judgement autodoc-judgement))
  (setf (autodoc-judgement-last-point judgement) nil
        (autodoc-judgement-modified-tick judgement) nil))


(defvar *autodoc-message* nil)

(defun clear-autodoc-message ()
  (reset-state (judgement-instance))
  (delete-popup-message *autodoc-message*))

(define-command lisp-autodoc () ()
  (let ((line-number (line-number-at-point (current-point)))
        (charpos (point-charpos (current-point))))
    (autodoc (lambda (buffer)
               ;; autodocのための情報はサーバーへの問い合わせがあり非同期なので
               ;; 情報が返ってきた時にはカーソルが別の場所にあって表示するべきではないことがある
               (when (and (= line-number (line-number-at-point (current-point)))
                          (= charpos (point-charpos (current-point))))
                 (setf *autodoc-message*
                       (display-popup-message buffer
                                              :timeout nil
                                              :destination-window *autodoc-message*)))
               (redraw-frame (current-frame))))))


(defun command-loop-autodoc ()
  (unless (should-continue-autodoc-p (judgement-instance) (current-point))
    (clear-autodoc-message)))


(defvar *autodoc-idle-timer* nil)

(defun autodoc-with-idle-timer ()
  (when (should-use-autodoc-p (judgement-instance) (current-point))
    (lisp-autodoc)))

(defun start-autodoc-idle-timer ()
  (unless *autodoc-idle-timer*
    (setf *autodoc-idle-timer* (start-idle-timer 1000 t 'autodoc-with-idle-timer))))

(defun stop-autodoc-idle-timer ()
  (when *autodoc-idle-timer*
    (stop-timer *autodoc-idle-timer*)
    (setf *autodoc-idle-timer* nil)))


(defun autodoc-enabled-p ()
  (not (null *autodoc-idle-timer*)))

(defun enable-autodoc ()
  (start-autodoc-idle-timer)
  (add-hook *post-command-hook* 'command-loop-autodoc)
  (values))

(defun disable-autodoc ()
  (stop-autodoc-idle-timer)
  (remove-hook *post-command-hook* 'command-loop-autodoc)
  (values))


(define-command lisp-insert-space-and-autodoc (n) ("p")
  (loop :repeat n :do (insert-character (current-point) #\space))
  (when (autodoc-enabled-p)
    (unless (continue-flag 'lisp-insert-space-and-autodoc)
      (lisp-autodoc))))


(add-hook *lisp-mode-hook* 'enable-autodoc)
