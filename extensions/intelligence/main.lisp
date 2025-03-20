(uiop:define-package #:lem-intelligence
  (:use #:cl
        #:lem)
  (:local-nicknames (#:ollama #:lem-intelligence/ollama)))
(in-package #:lem-intelligence)

(define-attribute process-region-attribute
  (t :background "dim gray" :foreground "white"))

(defun overlay-at (point)
  (dolist (overlay (point-overlays point))
    (when (overlay-get overlay 'intelligence-process)
      (return overlay))))

(defun interrupt-intelligence-process (overlay)
  (bt2:destroy-thread (overlay-get overlay 'thread))
  (lem/loading-spinner:stop-loading-spinner (overlay-get overlay 'spinner))
  (delete-overlay overlay))

(defun call-with-process-region (start end loading-message process then)
  (buffer-mark-cancel (point-buffer start))
  (let* ((overlay (make-overlay start end 'process-region-attribute))
         (spinner
           (lem/loading-spinner:start-loading-spinner 
            :line
            :loading-message loading-message
            :point end))
         (string (points-to-string start end))
         (thread (bt2:make-thread
                  (lambda ()
                    (let ((result (handler-bind ((error (lambda (e)
                                                          (declare (ignore e))
                                                          (send-event
                                                           (lambda ()
                                                             (lem/loading-spinner:stop-loading-spinner spinner)
                                                             (delete-overlay overlay))))))
                                    (funcall process string))))
                      (send-event (lambda ()
                                    (lem/loading-spinner:stop-loading-spinner spinner)
                                    (with-point ((start (overlay-start overlay) :left-inserting)
                                                 (end (overlay-end overlay) :right-inserting))
                                      (funcall then start end result)
                                      (delete-overlay overlay))))))
                  :name "lem-intelligence/process-region")))
    (overlay-put overlay 'spinner spinner)
    (overlay-put overlay 'thread thread)
    (overlay-put overlay 'intelligence-process t)
    (values)))

(define-command intelligence/cancel () ()
  (alexandria:when-let ((overlay (overlay-at (current-point))))
    (interrupt-intelligence-process overlay)))

(defun replace-text (start end text)
  (delete-between-points start end)
  (insert-string start text))

(defun display-buffer (start end text)
  (declare (ignore start end))
  (let ((buffer (make-buffer "*Intelligence*")))
    (erase-buffer buffer)
    (with-point ((point (buffer-point buffer) :left-inserting))
      (buffer-end point)
      (insert-string point text)
      (insert-character point #\newline))
    (pop-to-buffer buffer)))

;;; translator
(defparameter *default-translation-model* "phi4")

(defun translate (text &key (target-lang (alexandria:required-argument :target-lang))
                            (model *default-translation-model*))
  (ollama:slurp
   (ollama:generate
    (format nil
            "Please translate the following sentences into ~A
Then, please output only the translation results.
「~A」 "
            target-lang
            text)
    :model model)))

(defun translate-region (start end target-lang then)
  (call-with-process-region
   start
   end
   "Translation..."
   (lambda (string)
     (translate
      string
      :target-lang target-lang))
   then))

(define-command intelligence/translate-to-japanese (start end replace) (:region :universal-nil)
  (translate-region start end "japanese" (if replace #'replace-text #'display-buffer)))

(define-command intelligence/translate-to-english (start end replace) (:region :universal-nil)
  (translate-region start end "english" (if replace #'replace-text #'display-buffer)))

;;; coding
(defparameter *default-coding-model* "deepseek-coder-v2")

(defun make-prompt (code)
  (format nil "Could you please review the following code snippet and provide comments? Specifically, I'm interested in understanding:

1. The overall functionality and purpose of the code.
2. Any potential issues, such as bugs or inefficiencies.
3. Suggestions for improvements or best practices.

Here's the code:
```
~A
```" code))

(define-command intelligence/code-review (start end) (:region)
  (call-with-process-region
   start
   end
   "Review..."
   (lambda (string)
     (ollama:slurp
      (ollama:generate (make-prompt string) :model *default-coding-model*)))
   (lambda (start end result)
     (declare (ignore start end))
     (let ((buffer (make-buffer "*ollama*")))
       (erase-buffer buffer)
       (with-point ((point (buffer-point buffer) :left-inserting))
         (buffer-end point)
         (insert-string point result)
         (insert-character point #\newline))
       (pop-to-buffer buffer)
       (change-buffer-mode buffer 'lem-markdown-mode:markdown-mode)))))

(define-command intelligence/process-region (start end prompt replace)
    (:region
     (:string "Prompt: ")
     :universal-nil)
  (call-with-process-region
   start
   end
   "Processing..."
   (lambda (string)
     (ollama:slurp
      (ollama:generate (format nil "~A~%\"\"\"~A\"\"\"\"" prompt string)
                       :model "llama3.2")))
   (lambda (start end result)
     (if replace
         (replace-text start end result)
         (display-buffer start end result)))))
