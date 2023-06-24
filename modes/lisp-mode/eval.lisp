(defpackage :lem-lisp-mode/eval
  (:use :cl :lem :lem-lisp-mode/internal))
(in-package :lem-lisp-mode/eval)

(define-attribute eval-error-attribute
  (t :foreground "red" :bold t))

(define-attribute eval-value-attribute
  (t :foreground "sky blue" :bold t))

(define-key *lisp-mode-keymap* "C-x C-e" 'lisp-eval-at-point)
(define-key *lisp-mode-keymap* "C-c C-e" 'lisp-eval-at-point)
(define-key *lisp-mode-keymap* "C-c i" 'lisp-eval-interrupt-at-point)

(defun fold-one-line-message (message)
  (let ((pos (position #\newline message)))
    (if (not pos)
        message
        (format nil "~A..." (subseq message 0 pos)))))

(defun buffer-eval-result-overlays (buffer)
  (buffer-value buffer 'eval-result-overlays))

(defun (setf buffer-eval-result-overlays) (value buffer)
  (setf (buffer-value buffer 'eval-result-overlays) value))

(defun clear-eval-results (buffer)
  (mapc #'remove-eval-result-overlay
        (buffer-eval-result-overlays buffer)))

(defun overlay-eval-id (overlay)
  (overlay-get overlay :id))

(defun remove-eval-result-overlay (overlay)
  (let ((id (overlay-eval-id overlay)))
    (lisp-eval-async `(micros/pretty-eval:remove-evaluated-values ,id))
    (delete-overlay overlay)
    (delete-overlay (overlay-get overlay 'relation-overlay))
    (alexandria:removef (buffer-eval-result-overlays (overlay-buffer overlay))
                        overlay)))

(defun find-overlays (start end)
  (let ((buffer (point-buffer start)))
    (loop :for ov :in (buffer-eval-result-overlays buffer)
          :unless (or (point<= end (overlay-start ov))
                      (point<= (overlay-end ov) start))
          :collect ov)))

(defun find-overlay (point)
  (first (find-overlays point point)))

(defun remove-eval-result-overlay-between (start end)
  (dolist (ov (find-overlays start end))
    (remove-eval-result-overlay ov)))

(defun remove-touch-overlay (start arg)
  (with-point ((end start))
    (character-offset end
                      (etypecase arg
                        (string (length arg))
                        (character 0)
                        (integer arg)))
    (remove-eval-result-overlay-between start end)))

;; copied from src/display.lisp, TODO: extract this utils
(defun compute-evaluated-background-color ()
  (let ((color (parse-color (lem-core::background-color))))
    (multiple-value-bind (h s v)
        (rgb-to-hsv (color-red color)
                    (color-green color)
                    (color-blue color))
      (multiple-value-bind (r g b)
          (hsv-to-rgb h
                      s
                      (+ v 5))
        (format nil "#~X~X~X" r g b)))))

(defun display-spinner-message (spinner &optional message is-error id)
  (lem/loading-spinner:with-line-spinner-points (start end spinner)
    (let ((popup-overlay
            (make-overlay start 
                          end
                          (if is-error
                              'eval-error-attribute
                              'eval-value-attribute)
                          :start-point-kind :left-inserting
                          :end-point-kind :right-inserting))
          (background-overlay
            (make-overlay start
                          end
                          (make-attribute :background (compute-evaluated-background-color))
                          :start-point-kind :left-inserting
                          :end-point-kind :right-inserting))
         (buffer (point-buffer start)))
      (overlay-put popup-overlay 'relation-overlay background-overlay)
      (overlay-put popup-overlay :display-line-end t)
      (overlay-put popup-overlay :display-line-end-offset 1)
      (overlay-put popup-overlay :text (fold-one-line-message message))
      (overlay-put popup-overlay :id id)
      (push popup-overlay (buffer-eval-result-overlays buffer))
      (add-hook (variable-value 'before-change-functions :buffer buffer)
                'remove-touch-overlay))))

(defun spinner-eval-request-id (spinner)
  (lem/loading-spinner:spinner-value spinner 'eval-id))

(defun (setf spinner-eval-request-id) (eval-id spinner)
  (setf (lem/loading-spinner:spinner-value spinner 'eval-id) eval-id))

(defun eval-region (start end)
  (skip-whitespace-backward end)
  (remove-eval-result-overlay-between start end)
  (let ((spinner (lem/loading-spinner:start-loading-spinner :region :start start :end end))
        (string (points-to-string start end))
        (request-id (lem-lisp-mode/swank-protocol::new-request-id (current-connection))))
    (setf (spinner-eval-request-id spinner) request-id)
    (lem-lisp-mode/internal::with-remote-eval
        (`(micros/pretty-eval:pretty-eval ,string) :request-id request-id)
      (lambda (value)
        (alexandria:destructuring-ecase value
          ((:ok result)
           (destructuring-bind (&key value id) result
               (lem/loading-spinner:stop-loading-spinner spinner)
             (display-spinner-message spinner value nil id)))
          ((:abort condition)
           (lem/loading-spinner:stop-loading-spinner spinner)
           (display-spinner-message spinner condition t)))))))

(defun eval-last-expression (point)
  (with-point ((start point)
               (end point))
    (form-offset start -1)
    (eval-region start end)))

(define-command lisp-eval-at-point () ()
  (check-connection)
  (cond ((buffer-mark-p (current-buffer))
         (with-point ((start (region-beginning))
                      (end (region-end)))
           (eval-region start end)))
        (t
         (eval-last-expression (current-point)))))

(define-command lisp-eval-interrupt-at-point () ()
  (dolist (spinner (lem/loading-spinner:get-line-spinners (current-point)))
    (let ((request-id (spinner-eval-request-id spinner)))
      (lem-lisp-mode/swank-protocol::send-message (current-connection)
                                                  `(:interrupt-thread ,request-id)))))

(defmethod execute :around (mode (command lisp-inspect) argument)
  (let ((overlay (find-overlay (current-point))))
    (if overlay
        (let ((id (overlay-eval-id overlay)))
          (lisp-eval-async `(micros/pretty-eval:inspect-evaluation-value ,id)
                           'lem-lisp-mode/internal::open-inspector))
        (call-next-method))))

(define-command lisp-eval-clear () ()
  (clear-eval-results (current-buffer)))

(defun eval-print (string &optional print-right-margin)
  (let ((value (lisp-eval (if print-right-margin
                              `(let ((*print-right-margin* ,print-right-margin))
                                 (micros:eval-and-grab-output ,string))
                              `(micros:eval-and-grab-output ,string)))))
    (insert-string (current-point) (first value))
    (insert-character (current-point) #\newline)
    (insert-string (current-point) (second value))))

(define-command lisp-eval-last-expression-and-insert () ()
  (check-connection)
  (with-point ((start (current-point))
               (end (current-point)))
    (form-offset start -1)
    (run-hooks (variable-value 'before-eval-functions) start end)
    (let ((string (points-to-string start end)))
      (eval-print string)
      (move-point (current-point) end))))
