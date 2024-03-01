(in-package :lem/legit)

#|
Done:

- start a rebase process from the commit at point,
- open a rebase buffer and press p, f… to pick, fixup… the commit at point.
- validate it, stop it.

Nice to have:

- in the rebase buffer, show the commit diff on the right window, just like legit-status.
- create a major mode for git rebase files (we currently rely on yaml mode).
- prompt for confirmation in rebase-abort-yes-or-no

TODOs:

- when (e)dit or (r)eword are used, we need to handle another operation.
- show in legit-status when a rebase is in process.
- add commands to continue or abort the rebase in process.

and

- Windows support for the rebase script (trap a signal) (see porcelain).

|#

;; xxx: define a major mode for the git rebase file format,
;; where we would highlight special words and commits.
(define-major-mode legit-rebase-mode lem-yaml-mode:yaml-mode
    (:name "legit-rebase-mode"
     :syntax-table lem-yaml-mode::*yaml-syntax-table*
     :keymap *legit-rebase-mode-keymap*)
  (setf (variable-value 'enable-syntax-highlight) t))

;; Use commits with a keypress:
(define-key *legit-rebase-mode-keymap* "p" 'rebase-pick)
(define-key *legit-rebase-mode-keymap* "r" 'rebase-reword)
(define-key *legit-rebase-mode-keymap* "e" 'rebase-edit)
(define-key *legit-rebase-mode-keymap* "s" 'rebase-squash)
(define-key *legit-rebase-mode-keymap* "f" 'rebase-fixup)
(define-key *legit-rebase-mode-keymap* "x" 'rebase-exec)
(define-key *legit-rebase-mode-keymap* "b" 'rebase-break)
(define-key *legit-rebase-mode-keymap* "d" 'rebase-drop)
(define-key *legit-rebase-mode-keymap* "l" 'rebase-label)
(define-key *legit-rebase-mode-keymap* "t" 'rebase-reset)
(define-key *legit-rebase-mode-keymap* "m" 'rebase-merge)

;; Validate, abort.
(define-key *legit-rebase-mode-keymap* "C-c C-c" 'rebase-continue)
(define-key *legit-rebase-mode-keymap* "C-Return" 'rebase-continue)
(define-key *legit-rebase-mode-keymap* "M-q" 'rebase-abort)
(define-key *legit-rebase-mode-keymap* "C-c C-k" 'rebase-abort)
;; xxx: with validation.
(define-key *legit-rebase-mode-keymap* "Escape" 'rebase-abort-yes-or-no)
(define-key *legit-rebase-mode-keymap* "q" 'rebase-abort-yes-or-no)

;; Navigation.
(define-key *legit-rebase-mode-keymap* "n" 'next-line)
(define-key *legit-rebase-mode-keymap* "C-n" 'next-line)
(define-key *legit-rebase-mode-keymap* "C-p" 'previous-line)

;; Help.
(define-key *legit-rebase-mode-keymap* "?" 'rebase-help)
(define-key *legit-rebase-mode-keymap* "C-x ?" 'rebase-help)

(define-command rebase-continue () ()
  (run-function #'lem/porcelain::rebase-continue)
  (kill-buffer "git-rebase-todo"))

(define-command rebase-abort () ()
  (run-function #'lem/porcelain::rebase-abort)
  (kill-buffer "git-rebase-todo"))

(define-command rebase-abort-yes-or-no () ()
  ;; TODO: prompt for confirmation.
  (run-function #'lem/porcelain::rebase-kill)
  (kill-buffer "git-rebase-todo"))

(defun %rebase-change-command (command)
  "Insert this command (string, such as \"fixup\") at the beginning of this line."
  (save-excursion
    (move-to-beginning-of-line)
    (delete-word 1)
    (insert-string (current-point) command)
    (save-buffer (current-buffer)))
  (move-to-next-virtual-line (current-point)))

(define-command rebase-pick () ()
  (%rebase-change-command "pick"))

(define-command rebase-reword () ()
  (%rebase-change-command "reword"))

(define-command rebase-edit () ()
  (%rebase-change-command "edit"))

(define-command rebase-squash () ()
  (%rebase-change-command "squash"))

(define-command rebase-fixup () ()
  (%rebase-change-command "fixup"))

(define-command rebase-exec () ()
  (%rebase-change-command "exec"))

(define-command rebase-break () ()
  (%rebase-change-command "break"))

(define-command rebase-drop () ()
  (%rebase-change-command "drop"))

(define-command rebase-label () ()
  (%rebase-change-command "label"))

(define-command rebase-reset () ()
  (%rebase-change-command "reset"))

(define-command rebase-merge () ()
  (%rebase-change-command "merge"))

(define-command rebase-help () ()
  "Show the important keybindings."
  (with-pop-up-typeout-window (s (make-buffer "*legit-help*") :erase t)
    (format s "Git interactive rebase.~&")
    (format s "~%")
    (format s "Commands:~&")
    (format s "(p)ick commit, (f)ixup... WARN: other commands like reword are not implemented.")
    (format s "~%")
    (format s "Validate: C-Return, C-c C-c~&")
    (format s "Stop and quit: Escape, M-q.~&")
    (format s "Navigate: C-n and C-p.~&")
    (format s "~%")
    (format s "Show this help: C-x ? or ?, M-x legit-rebase-help")
    ))
