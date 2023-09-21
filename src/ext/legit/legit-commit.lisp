(in-package :lem/legit)

#|
Done:

- "c" opens a commit message window on the right side, we can type a long message,
- lines starting by a # are ignored.

TODOs:

- add C-c C-s to sign the message
- and other shortcuts and features

Future:

- save previous messages, add C-p C-n commands to show them
- find related Github issue when writing "fixes #"

|#

(define-major-mode legit-commit-mode lem-markdown-mode:markdown-mode
    (:name "legit-commit-mode"
     :syntax-table lem-markdown-mode::*markdown-syntax-table*
     :keymap *legit-commit-mode-keymap*)
  ;; no syntax highlihgt in fact.
  (setf (variable-value 'enable-syntax-highlight) t))

;; Validate, abort.
(define-key *legit-commit-mode-keymap* "C-c C-c" 'commit-continue)
(define-key *legit-commit-mode-keymap* "C-Return" 'commit-continue)
(define-key *legit-commit-mode-keymap* "M-q" 'commit-abort)
(define-key *legit-commit-mode-keymap* "C-c C-k" 'commit-abort)

;; Navigation.
;; find and display the previous commit messages.
;; (define-key *legit-commit-mode-keymap* "C-n" 'next-commit)
;; (define-key *legit-commit-mode-keymap* "C-p" 'previous-commit)

;; Help.
(define-key *legit-commit-mode-keymap* "C-x ?" 'commit-help)

(defun commit ()
  (let ((buffer (make-buffer "*legit-commit*")))
    (setf (buffer-directory buffer) (uiop:getcwd))
    (setf (buffer-read-only-p buffer) nil)
    (erase-buffer buffer)
    (move-to-line (buffer-point buffer) 1)
    (insert-string (buffer-point buffer) "write commit message:")
    (change-buffer-mode buffer 'legit-commit-mode)
    ;; (setf (buffer-read-only-p buffer) t)
    (move-to-line (buffer-point buffer) 1)))

(defun clean-commit-message (text)
  "Remove lines starting with a #."
  ;; We should collect meaningful data too, like a signature.
  (loop for line in (str:lines text)
        unless (str:starts-with-p "#" line)
          collect line into result
        finally (return (str:unlines result))))

;; void message:
#+(or)
(assert (str:blankp (clean-commit-message
"# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
")))

;; a message on the first line:
#+(or)
(assert (equal "test message" (clean-commit-message
"test message
# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
")))

;; a few lines:
#+(or)
(assert (equal "one

two
" (clean-commit-message
"one

two

# Please enter the commit message for your changes. Lines starting
# with '#' will be ignored, and an empty message aborts the commit.
")))

(defun commit-continue ()
  (let* ((message (buffer-text (make-buffer "*legit-commit*")))
         (cleaned-message (clean-commit-message message)))
    (cond
      ((str:blankp cleaned-message)
       (message "No commit message, do nothing."))
      (t
       (with-current-project ()
         (run-function (lambda ()
                         (lem/porcelain::commit cleaned-message))
                       :message "commited")
         (kill-buffer "*legit-commit*")
         ;; come back on the status on  the left:
         (lem-core/commands/window:previous-window)
         ;; and refresh.
         (legit-status))))))

(define-command commit-abort () ()
  (kill-buffer "*legit-commit*")
  (lem-core/commands/window:previous-window))

(define-command commit-abort-yes-or-no () ()
  ;; TODO: prompt for confirmation.
  (kill-buffer "*legit-commit*"))

(define-command commit-help () ()
  "Show the important keybindings."
  (with-pop-up-typeout-window (s (make-buffer "*legit-help*") :erase t)
    (format s "Legit commit.~&")
    (format s "~%")
    (format s "Commands:~&")
    (format s "Validate: C-Return, C-c C-c~&")
    (format s "Stop and quit: Escape, M-q.~&")
    (format s "~%")
    (format s "Show this help: C-x ? or ?, M-x legit-commit-help")
    ))
