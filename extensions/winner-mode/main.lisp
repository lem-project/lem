(defpackage #:lem-winner-mode
  (:use #:cl #:lem)
  (:export #:winner-mode
           #:winner-undo
           #:winner-redo
           #:*winner-max-configurations*
           #:*winner-mode-keymap*))
(in-package #:lem-winner-mode)

(defparameter *winner-max-configurations* 200
  "Maximum number of window configurations remembered per frame.")

(defvar *winner-frame-states* '()
  "Alist of (frame . winner-frame-state) holding the layout history of each frame.

This is the only piece of state that is not passed around as an argument,
because `winner-record' runs from `*post-command-hook*' without arguments.")

(defvar *winner-last-command* nil
  "The command that ran last.
Used to tell whether a sequence of `winner-undo' is in progress, so that the
layouts shown during it can be skipped (see `winner-step').")

(defvar *winner-mode-keymap* (make-keymap))

(define-key *winner-mode-keymap* "C-c Left" 'winner-undo)
(define-key *winner-mode-keymap* "C-c Right" 'winner-redo)

;;; A saved configuration keeps the window tree structure, the geometry and the
;;; buffer/point of every window, so that the layout can be rebuilt later.

(defstruct winner-leaf
  buffer      ; the buffer the window was displaying
  point       ; marker: the point the window was showing
  view-point  ; marker: the point the window was scrolled to
  x y width height
  current-p)  ; was this the selected window when the layout was saved?

(defstruct winner-node
  split-type  ; :vsplit or :hsplit
  left right) ; a `winner-leaf' or another `winner-node'

(defstruct winner-config
  frame
  tree        ; a `winner-leaf' or a `winner-node'
  area)       ; the frame's area, see `frame-area'

(defstruct winner-frame-state
  undo-stack    ; configurations to undo to, most recent first
  redo-stack    ; configurations to redo to, most recent first
  last-config   ; the layout `winner-record' saw last
  last-win-data ; `layout-data' of `last-config', so it can be compared
  seen-win-data); layouts already shown during the current undo sequence

;;; The window tree node accessors are not exported from `lem-core'
;;; (src/internal-packages.lisp re-exports only `balance-windows' out of
;;; window-tree.lisp), but saving and replaying a layout means walking the tree
;;; structure itself, so there is no way around them.  They are wrapped here to
;;; keep `lem-core::' from being scattered all over the file.

(defun tree-node-split-type (node)
  "Return the split type of the window tree NODE, `:vsplit' or `:hsplit'."
  (lem-core::window-node-split-type node))

(defun tree-node-left (node)
  "Return the window or subtree on the top (left) side of the tree NODE."
  (lem-core::window-node-left node))

(defun tree-node-right (node)
  "Return the window or subtree on the bottom (right) side of the tree NODE."
  (lem-core::window-node-right node))

(defun frame-area (&optional (frame (current-frame)))
  "Return the screen area FRAME lays its windows out in."
  (list (topleft-window-x frame)
        (topleft-window-y frame)
        (max-window-width frame)
        (max-window-height frame)))

(defun tree-leaves (node)
  "Return the saved windows of NODE (a `winner-leaf' or a `winner-node')."
  (if (winner-leaf-p node)
      (list node)
      (append (tree-leaves (winner-node-left node))
              (tree-leaves (winner-node-right node)))))

(defun layout-entry< (entry1 entry2)
  "Order two layout entries of `layout-data' consistently."
  (loop :for value1 :in entry1
        :for value2 :in entry2
        :do (cond ((and (numberp value1) (numberp value2))
                   (unless (= value1 value2)
                     (return (< value1 value2))))
                  ((not (equal value1 value2))
                   (return (string< (buffer-name value1)
                                    (buffer-name value2)))))))

(defun layout-data (frame)
  "Return a cheap description of FRAME's window layout that can be compared.

Two layouts are `equal' when every window displays the same buffer in the same
place with the same size, like Emacs' `winner-win-data'.  Point positions are
deliberately left out, so that moving the cursor or editing never looks like a
layout change."
  (sort (loop :for window :in (window-list frame)
              :collect (list (window-x window)
                             (window-y window)
                             (window-width window)
                             (window-height window)
                             (window-buffer window)))
        #'layout-entry<))

(defun config-layout-data (config)
  "Return the layout description of the saved configuration CONFIG."
  (sort (loop :for leaf :in (tree-leaves (winner-config-tree config))
              :collect (list (winner-leaf-x leaf)
                             (winner-leaf-y leaf)
                             (winner-leaf-width leaf)
                             (winner-leaf-height leaf)
                             (winner-leaf-buffer leaf)))
        #'layout-entry<))

(defun capture-tree (node selected-window)
  "Save NODE, a live window or window tree node, as a `winner-leaf'/`winner-node'."
  (if (windowp node)
      (make-winner-leaf :buffer (window-buffer node)
                        :point (copy-point (window-point node) :right-inserting)
                        :view-point (copy-point (window-view-point node) :right-inserting)
                        :x (window-x node)
                        :y (window-y node)
                        :width (window-width node)
                        :height (window-height node)
                        :current-p (eq node selected-window))
      (make-winner-node :split-type (tree-node-split-type node)
                        :left (capture-tree (tree-node-left node) selected-window)
                        :right (capture-tree (tree-node-right node) selected-window))))

(defun save-config (&optional (frame (current-frame)))
  "Save FRAME's current window configuration.

The configuration owns its markers; free it with `discard-config' once it is
dropped from the history."
  (let ((selected-window (and (eq frame (current-frame))
                              (current-window))))
    (make-winner-config :frame frame
                        :tree (capture-tree (frame-window-tree frame) selected-window)
                        :area (frame-area frame))))

(defun discard-config (config)
  "Free the markers held by CONFIG."
  (when config
    (loop :for leaf :in (tree-leaves (winner-config-tree config))
          :do (loop :for marker :in (list (winner-leaf-point leaf)
                                          (winner-leaf-view-point leaf))
                    ;; `delete-point' would fail on a point whose line is gone
                    ;; (its buffer was killed), and such a point is not
                    ;; registered anywhere anymore.
                    :when (alive-point-p marker)
                      :do (delete-point marker)))))

(defun clear-configs (stack)
  "Free every configuration in STACK and return an empty stack."
  (loop :for config :in stack
        :do (discard-config config))
  '())

(defun add-config (stack config)
  "Return STACK with CONFIG pushed onto it.

Configurations past `*winner-max-configurations*' are dropped, oldest first."
  (let ((stack (if config (cons config stack) stack)))
    (loop :while (> (length stack) *winner-max-configurations*)
          :do (discard-config (car (last stack)))
              (setf stack (butlast stack)))
    stack))

(defun frame-state (frame)
  "Return FRAME's winner state, or nil if nothing was recorded for it yet."
  ;; `assoc' compares with `eql' by default, which is identity for frames.
  (cdr (assoc frame *winner-frame-states*)))

(defun ensure-frame-state (frame)
  "Return FRAME's winner state, creating it if needed."
  (or (frame-state frame)
      (let ((state (make-winner-frame-state)))
        (push (cons frame state) *winner-frame-states*)
        state)))

(defun clear-frame-state (frame)
  "Free everything recorded for FRAME and forget it."
  (alexandria:when-let ((state (frame-state frame)))
    (discard-config (winner-frame-state-last-config state))
    (clear-configs (winner-frame-state-undo-stack state))
    (clear-configs (winner-frame-state-redo-stack state))
    (setf *winner-frame-states*
          (remove frame *winner-frame-states* :key #'car))))

(defun reset-frame-state (frame)
  "Forget FRAME's history and start over from its current layout."
  (let ((state (ensure-frame-state frame)))
    (discard-config (winner-frame-state-last-config state))
    (clear-configs (winner-frame-state-undo-stack state))
    (clear-configs (winner-frame-state-redo-stack state))
    (setf (winner-frame-state-undo-stack state) '()
          (winner-frame-state-redo-stack state) '()
          (winner-frame-state-seen-win-data state) '()
          (winner-frame-state-last-config state) (save-config frame)
          (winner-frame-state-last-win-data state) (layout-data frame))))

(defun prune-frame-states ()
  "Forget the layout history of frames that no longer exist."
  (loop :for frame :in (mapcar #'car *winner-frame-states*)
        :unless (member frame (all-frames) :test #'eq)
          :do (clear-frame-state frame)))

(defun winner-record ()
  "Record the layout that the command which just ran changed.

Runs from `*post-command-hook*' while Winner mode is on.  An error signalled
here would escape to the command loop as a backtrace on every single command,
so nothing is allowed to get out."
  (ignore-errors
    (let* ((frame (current-frame))
           (state (ensure-frame-state frame))
           (data (layout-data frame)))
      (alexandria:when-let ((command (this-command)))
        (setf *winner-last-command* (command-name command)))
      (unless (equal data (winner-frame-state-last-win-data state))
        (prune-frame-states)
        ;; The layout we were showing before this command goes on the undo
        ;; stack; a new change also invalidates the redo history and ends any
        ;; undo sequence that was in progress.
        (setf (winner-frame-state-undo-stack state)
              (add-config (winner-frame-state-undo-stack state)
                          (winner-frame-state-last-config state)))
        (setf (winner-frame-state-redo-stack state)
              (clear-configs (winner-frame-state-redo-stack state)))
        (setf (winner-frame-state-seen-win-data state) '()
              (winner-frame-state-last-config state) (save-config frame)
              (winner-frame-state-last-win-data state) data)))))

(defun region-size (node)
  "Return the screen size of the area NODE's saved windows cover, as two values.

This is the bounding box of the leaves, not the sum of their sizes: the leaves
of one side of a nested split sit next to each other, so adding them up would
count the same rows (or columns) twice.  The margins between the siblings are
included, which is just what `split-window-*' expects for its first child."
  (let ((leaves (tree-leaves node)))
    (values (- (reduce #'max leaves
                       :key (lambda (leaf)
                              (+ (winner-leaf-x leaf) (winner-leaf-width leaf))))
               (reduce #'min leaves :key #'winner-leaf-x))
            (- (reduce #'max leaves
                       :key (lambda (leaf)
                              (+ (winner-leaf-y leaf) (winner-leaf-height leaf))))
               (reduce #'min leaves :key #'winner-leaf-y)))))

(defun region-width (node)
  "Return the width of the area NODE's saved windows cover."
  (nth-value 0 (region-size node)))

(defun region-height (node)
  "Return the height of the area NODE's saved windows cover."
  (nth-value 1 (region-size node)))

(defun collapse-to-one-window ()
  "Delete every window of the current frame except one that can be switched.

Returns that window.  Restoring a configuration starts from a single window,
because new windows can only be made by splitting an existing one."
  (let ((window (find-if (lambda (window)
                           ;; A window showing a buffer that must not be
                           ;; switched away from would make the whole restore
                           ;; fail at the first `switch-to-buffer'.
                           (not (not-switchable-buffer-p (window-buffer window))))
                         (window-list (current-frame)))))
    (unless window
      (editor-error "No window to restore the configuration into"))
    (switch-to-window window)
    ;; `delete-other-windows' is a command, but commands are plain functions
    ;; here.  It takes care of attached and floating windows and stretches the
    ;; remaining window over the whole frame.
    (delete-other-windows)
    (current-window)))

(defun rebuild-tree (node window)
  "Recreate NODE's split structure inside WINDOW.

Returns an alist mapping every saved leaf to the live window displaying it.
The tree is rebuilt in pre-order because `split-window-*' replaces a window
with a split of it, so the structure can only be built from the top down."
  (if (winner-leaf-p node)
      (list (cons node window))
      (let* ((before (window-list (current-frame)))
             (new-window
               (progn
                 (ecase (winner-node-split-type node)
                   (:vsplit
                    (split-window-vertically window
                                             :height (region-height (winner-node-left node))))
                   (:hsplit
                    (split-window-horizontally window
                                               :width (region-width (winner-node-left node)))))
                 ;; The split functions return T rather than the window they
                 ;; made, and `get-next-window' can hand back an attached
                 ;; window, so pick out the one window that is new.
                 (find-if-not (lambda (live) (member live before))
                              (window-list (current-frame))))))
        (append (rebuild-tree (winner-node-left node) window)
                (rebuild-tree (winner-node-right node) new-window)))))

(defun restore-buffers (window-table)
  "Switch every window in WINDOW-TABLE back to its saved buffer.

`switch-to-buffer' is the only public way to change the buffer of a window that
is not the current one, and it recreates the window's points while at it, which
is why the points are restored afterwards."
  (loop :for (leaf . window) :in window-table
        :do (with-current-window window
              ;; RECORD and MOVE-PREV-POINT are nil so that the buffer order
              ;; and the saved point of the previous buffer are left alone.
              (switch-to-buffer (winner-leaf-buffer leaf) nil nil))))

(defun restore-points (window-table)
  "Move every window in WINDOW-TABLE back to its saved point and view point.

`switch-to-buffer' leaves every window at the beginning of its buffer, so a
saved marker that is not usable anymore can simply be left alone."
  (loop :for (leaf . window) :in window-table
        :do (let ((buffer (window-buffer window)))
              (when (and (alive-point-p (winner-leaf-point leaf))
                         (eq (point-buffer (winner-leaf-point leaf)) buffer))
                (move-point (window-point window) (winner-leaf-point leaf)))
              (when (and (alive-point-p (winner-leaf-view-point leaf))
                         (eq (point-buffer (winner-leaf-view-point leaf)) buffer))
                (move-point (window-view-point window) (winner-leaf-view-point leaf))))))

(defun restore-geometry (window-table config)
  "Give every window in WINDOW-TABLE the position and size it had in CONFIG.

Splitting already produces the saved layout as long as the frame is the same
size as it was when the configuration was saved, so this only patches up the
leftovers."
  (if (equal (winner-config-area config) (frame-area))
      ;; The setters skip windows that are already correct, so this does not
      ;; trigger needless redraws and size change hooks.
      (let ((lem-core::*update-only-when-state-changed* t))
        (loop :for (leaf . window) :in window-table
              ;; `window-set-size' refuses a width of 2 and less, but the saved
              ;; configuration is a real one and may well contain such a window.
              :when (and (< 2 (winner-leaf-width leaf))
                         (plusp (winner-leaf-height leaf)))
                :do (window-set-pos window (winner-leaf-x leaf) (winner-leaf-y leaf))
                    (window-set-size window
                                     (winner-leaf-width leaf)
                                     (winner-leaf-height leaf))))
      ;; The frame changed size (the terminal was resized, for instance), so
      ;; the saved sizes do not fit anymore.  Spread the windows evenly.
      (balance-windows)))

(defun choose-window (window-table buffer)
  "Return the window of WINDOW-TABLE to select after a restore.

Prefer the window that displayed BUFFER right before the restore, like Emacs,
which keeps the selected window.  Fall back to the window that was selected
when the layout was saved, and then to the first window."
  (or (loop :for (leaf . window) :in window-table
            :when (eq (winner-leaf-buffer leaf) buffer)
              :do (return window))
      (loop :for (leaf . window) :in window-table
            :when (winner-leaf-current-p leaf)
              :do (return window))
      (cdr (first window-table))))

(defun restore-config (config)
  "Restore the window configuration CONFIG in the current frame."
  (unless (eq (winner-config-frame config) (current-frame))
    (editor-error "This window configuration was saved for another frame"))
  ;; Remember the selected buffer, so that the window showing it is selected
  ;; again afterwards.
  (let* ((buffer (window-buffer (current-window)))
         (root (collapse-to-one-window))
         (window-table (rebuild-tree (winner-config-tree config) root)))
    (restore-buffers window-table)
    (restore-points window-table)
    (restore-geometry window-table config)
    ;; Select last: `(setf (current-window))' copies the point of the newly
    ;; selected window into its buffer.
    (switch-to-window (choose-window window-table buffer))
    (clear-screens-of-window-list)
    (redraw-display)))

(defun config-usable-p (config)
  "Return T if CONFIG can still be restored.

A configuration is useless once one of its buffers has been killed or has been
made not switchable in the meantime; Emacs' `winner-undo' skips those too."
  (loop :for leaf :in (tree-leaves (winner-config-tree config))
        :always (let ((buffer (winner-leaf-buffer leaf)))
                  (and (not (deleted-buffer-p buffer))
                       (not (not-switchable-buffer-p buffer))))))

(defun pop-usable-config (stack seen)
  "Pop configurations from STACK until one can be restored.

Configurations that cannot be restored anymore, and the ones whose layout is
already in SEEN (the layouts shown since the current undo sequence began), are
dropped, the way Emacs' `winner-undo-this' discharges them.  Returns two
values: the configuration to restore, or nil, and the remaining stack."
  (loop :while (and stack
                    (let ((config (first stack)))
                      (or (not (config-usable-p config))
                          (member (config-layout-data config) seen :test #'equal))))
        :do (discard-config (pop stack)))
  (values (pop stack) stack))

(defun winner-step (direction)
  "Move one step through the window configuration history.

DIRECTION is `:undo' for the configuration before the last change, or `:redo'
for the one after it."
  (unless (mode-active-p (current-buffer) 'winner-mode)
    (editor-error "Winner mode is turned off"))
  (let* ((frame (current-frame))
         (state (ensure-frame-state frame))
         (undo-p (eq direction :undo))
         ;; A run of undos must not show the same layout twice, so remember the
         ;; layouts shown since the run started (Emacs' `winner-undone-data').
         (seen (and undo-p
                    (if (eq *winner-last-command* 'winner-undo)
                        (winner-frame-state-seen-win-data state)
                        (list (layout-data frame)))))
         (source (if undo-p
                     (winner-frame-state-undo-stack state)
                     (winner-frame-state-redo-stack state))))
    (multiple-value-bind (target rest) (pop-usable-config source seen)
      ;; Drop the configurations that were passed over, so they are not looked
      ;; at again on the next call.
      (if undo-p
          (setf (winner-frame-state-undo-stack state) rest)
          (setf (winner-frame-state-redo-stack state) rest))
      (unless target
        (editor-error "No ~:[further~;previous~] window configuration" undo-p))
      ;; The current layout is only saved now: restoring replaces it, and a
      ;; failed undo should not leave an entry behind.
      (if undo-p
          (setf (winner-frame-state-redo-stack state)
                (add-config (winner-frame-state-redo-stack state) (save-config frame)))
          (setf (winner-frame-state-undo-stack state)
                (add-config (winner-frame-state-undo-stack state) (save-config frame))))
      (restore-config target)
      ;; Take the restored layout for the recorded one, so that the post
      ;; command hook does not record this undo as a change of its own.
      (let ((data (layout-data frame)))
        (setf (winner-frame-state-last-config state) (save-config frame)
              (winner-frame-state-last-win-data state) data
              (winner-frame-state-seen-win-data state) (if undo-p
                                                           (cons data seen)
                                                           '())))
      (setf *winner-last-command* (if undo-p 'winner-undo 'winner-redo))
      (message "Winner ~:[redo~;undo~]" undo-p))))

(define-command winner-undo () ()
  "Undo the last change to the window configuration.

Restores the layout that was in effect before the most recent change to it.
Calling it again steps further back.  `winner-redo' steps forward again."
  (winner-step :undo))

(define-command winner-redo () ()
  "Redo the window configuration change that `winner-undo' undid.

Only the undo history is redone: any other change to the window layout
discards the redo history."
  (winner-step :redo))

(defun enable ()
  "Start recording changes to the window configurations."
  (prune-frame-states)
  (loop :for frame :in (all-frames)
        :do (reset-frame-state frame))
  (add-hook *post-command-hook* 'winner-record))

(defun disable ()
  "Stop recording window configuration changes and drop the history."
  (remove-hook *post-command-hook* 'winner-record)
  (loop :for frame :in (mapcar #'car *winner-frame-states*)
        :do (clear-frame-state frame))
  (setf *winner-last-command* nil))

(define-minor-mode winner-mode
    (:name "Winner"
     :description "Records window configurations so they can be undone."
     :global t
     :keymap *winner-mode-keymap*
     :enable-hook 'enable
     :disable-hook 'disable))
