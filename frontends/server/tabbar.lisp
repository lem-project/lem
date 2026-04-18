(defpackage :lem/tabbar
  (:use :cl :lem :lem/button)
  (:export :tabbar-active-tab-attribute
           :tabbar-attribute
           :tabbar-background-attribute
           :tabbar)
  #+sbcl
  (:lock t))
(in-package :lem/tabbar)

(define-attribute tabbar-active-tab-attribute
  (t :foreground "black" :background "dark gray"))

(define-attribute tabbar-attribute
  (t :foreground "white" :background "gray"))

(define-attribute tabbar-background-attribute
  (t :underline t))

(defclass tabbar-window (header-window)
  ((buffer
    :initarg :buffer
    :accessor tabbar-buffer)
   (need-update
    :initform nil
    :accessor tabbar-need-update-p)
   (buffer-list
    :accessor tabbar-window-buffer-list)))

(defmethod header-window-height ((window tabbar-window))
  2)

(defvar *tabbar* nil)

(defun tabbar-init ()
  (let ((buffer (make-buffer "*tabbar*" :temporary t :enable-undo-p nil)))
    (change-class buffer 'html-buffer :html "")
    (setf *tabbar* (make-instance 'tabbar-window :buffer buffer))))

(defun get-tabbar-buffers ()
  (handler-case
      (progn
        (unless (slot-boundp *tabbar* 'buffer-list)
          (setf (tabbar-window-buffer-list *tabbar*) (buffer-list))
          (return-from get-tabbar-buffers
            (tabbar-window-buffer-list *tabbar*)))
        (let ((buffer-list (tabbar-window-buffer-list *tabbar*)))
          (dolist (removed-buffer (set-difference buffer-list (buffer-list)))
            (setf buffer-list (remove removed-buffer buffer-list)))
          (setf buffer-list (append buffer-list (set-exclusive-or buffer-list (buffer-list))))
          (setf (tabbar-window-buffer-list *tabbar*) buffer-list)
          buffer-list))
    (error (e)
      (message "~A" e))))

(defun generate-html ()
  (let ((editor-bg (let ((bg (lem:background-color)))
                    (if bg
                        (lem:color-to-hex-string (lem:parse-color bg))
                        "#2d2d2d"))))
    (with-output-to-string (out)
    (format out "<head>")
    (format out "<style>
/* ===== Theme tokens ===== */
:root {
  --tab-bg: #181818;
  --tab-bg-hover: #252525;
  --tab-bg-active: ~A;
  --tab-fg: #8b8b8b;
  --tab-fg-active: #e0e0e0;
  --tab-border: #2a2a2a;
  --tab-accent: #4cc2ff; /* Zed-like cool color */
  --tab-dirty: #e5c07b; /* Orange for modified buffers */
}" editor-bg)
    (write-line "

/* ===== Page base ===== */
html, body {
  margin: 0; padding: 0;
  width: 100%; height: 100%;
  background: var(--tab-bg);
  color: var(--tab-fg);
  -webkit-font-smoothing: antialiased;
  overflow: hidden;
}

/* ===== Tabbar container ===== */
.lem-editor__tabbar {
  display: flex;
  align-items: stretch;
  height: 100%;
  box-sizing: border-box;
  padding: 0;
  margin: 0;
  background: var(--tab-bg);
  box-shadow: inset 0 -1px 0 0 var(--tab-border);
  overflow-x: auto;
  overflow-y: hidden;
  scrollbar-width: none;
}
.lem-editor__tabbar::-webkit-scrollbar { display: none; }

/* ===== Tab button ===== */
.lem-editor__tabbar-button {
  position: relative;
  display: inline-flex;
  align-items: center;
  gap: 6px;
  background: transparent;
  border: none;
  border-right: 1px solid var(--tab-border);
  color: var(--tab-fg);
  padding: 0 14px;
  width: fit-content;
  font: 500 12px/1 ui-sans-serif, system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial;
  letter-spacing: .01em;
  cursor: pointer;
  white-space: nowrap;
  overflow: hidden;
  text-overflow: ellipsis;
  transition: background .1s ease, color .1s ease;
  flex-shrink: 0;
}

.lem-editor__tabbar-button:last-child {
  border-right: none;
}

/* hover */
.lem-editor__tabbar-button:hover {
  background: var(--tab-bg-hover);
  color: var(--tab-fg-active);
}

/* active tab */
.lem-editor__tabbar-button.active {
  background: var(--tab-bg-active);
  color: var(--tab-fg-active);
  box-shadow: inset 0 1px 0 0 var(--tab-accent);
}



/* focus ring */
.lem-editor__tabbar-button:focus-visible {
  outline: none;
  box-shadow: inset 0 0 0 1px rgba(59,130,246,.5);
}

/* close icon */
.tab-close {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  visibility: hidden;
  width: 16px;
  height: 16px;
  border-radius: 3px;
  border: none;
  background: transparent;
  color: var(--tab-fg);
  font-size: 14px;
  line-height: 16px;
  text-align: center;
  cursor: pointer;
  padding: 0;
  flex-shrink: 0;
}
.lem-editor__tabbar-button:hover .tab-close,
.lem-editor__tabbar-button.active .tab-close {
  visibility: visible;
}
.tab-close:hover {
  background: rgba(255,255,255,.1);
  color: var(--tab-fg-active);
}

/* modified dot */
.tab-dirty {
  width: 7px;
  height: 7px;
  border-radius: 50%;
  background: var(--tab-dirty);
  flex-shrink: 0;
  visibility: hidden;
}
.lem-editor__tabbar-button.dirty .tab-dirty {
  visibility: visible;
}


/* disabled */
.lem-editor__tabbar-button[disabled] {
  opacity: .4;
  cursor: default;
  pointer-events: none;
}
</style>
" out)
    (format out "</head>~%")
    (format out "<body>~%")
    (format out "<div class='lem-editor__tabbar'>~%")
    (loop :for buffer :in (get-tabbar-buffers)
          :for active-p := (eq buffer (current-buffer))
          :for dirty-p := (buffer-modified-p buffer)
          :for name := (buffer-name buffer)
          :do (format out
                      "<button class=\"lem-editor__tabbar-button~A~A\" data-name=\"~A\">"
                      (if active-p " active" "")
                      (if dirty-p " dirty" "")
                      name)
              (format out "<span class=\"tab-dirty\"></span>")
              (format out "<span class=\"tab-label\">~A</span>" name)
              (format out "<span class=\"tab-close\">×</span>")
              (format out "</button>~%"))
    (format out "</div>~%")
    (write-line "
<script>
function handleClick(name) {
  invokeLem('tabbar/select', name);
}
function handleClose(name) {
  invokeLem('tabbar/close', name);
}
const buttons = document.querySelectorAll('.lem-editor__tabbar-button');
buttons.forEach(button => {
  button.addEventListener('mousedown', (e) => {
    e.preventDefault();
  });
  button.addEventListener('touchstart', (e) => {
    e.preventDefault();
  }, { passive: false });
  button.addEventListener('click', (e) => {
    const btn = e.target.closest('.lem-editor__tabbar-button');
    if (!btn) return;
    if (e.target.closest('.tab-close')) {
      handleClose(btn.dataset.name);
    } else {
      handleClick(btn.dataset.name);
    }
  });
});
</script>"
                out)
    (format out "</body>~%"))))

(lem-server:register-method
 "tabbar/select"
 (lambda (buffer-name)
   (send-event (lambda ()
                 (alexandria:when-let (buffer (get-buffer buffer-name))
                   (change-to-buffer buffer)
                   (redraw-display))))))

(lem-server:register-method
 "tabbar/close"
 (lambda (buffer-name)
   (send-event (lambda ()
                 (alexandria:when-let (buffer (get-buffer buffer-name))
                   (kill-buffer buffer)
                   (redraw-display))))))

(defun change-to-buffer (buffer)
  (dolist (window (window-list))
    (when (eq (window-buffer window) buffer)
      (switch-to-window window)
      (return-from change-to-buffer)))
  (switch-to-buffer buffer))

(defun update (&rest args)
  (declare (ignore args))
  (when *tabbar*
    (setf (tabbar-need-update-p *tabbar*) nil)))

(add-hook *switch-to-buffer-hook* 'update)
(add-hook *switch-to-window-hook* 'update)
(add-hook (variable-value 'kill-buffer-hook :global t) 'update)
(add-hook (variable-value 'after-change-functions :global) 'update)
(add-hook (variable-value 'after-save-hook :global) 'update)

(defmethod window-redraw ((window tabbar-window) force)
  (declare (ignore force))
  (unless (tabbar-need-update-p window)
    (lem-server::change-view-to-html window (generate-html))
    (setf (tabbar-need-update-p window) t)))

(defun tabbar-off ()
  (when (and (variable-value 'tabbar :global)
             *tabbar*)
    (delete-window *tabbar*)
    (setf *tabbar* nil)))

(defun tabbar-on ()
  (unless (variable-value 'tabbar :global)
    (tabbar-init)))

(define-editor-variable tabbar nil ""
  (lambda (value)
    (if value
        (tabbar-on)
        (tabbar-off))))

(define-command toggle-tabbar () ()
  (setf (variable-value 'tabbar :global)
        (not (variable-value 'tabbar :global))))

(define-key *global-keymap* "Shift-PageDown" 'tabbar-next)
(define-key *global-keymap* "Shift-PageUp" 'tabbar-prev)

(define-command tabbar-next () ()
  (let ((buffers (get-tabbar-buffers)))
    (alexandria:when-let (buffer (or (cadr (member (current-buffer) buffers))
                                     (first buffers)))
      (change-to-buffer buffer))))

(define-command tabbar-prev () ()
  (let ((buffers (reverse (get-tabbar-buffers))))
    (alexandria:when-let (buffer (or (cadr (member (current-buffer) buffers))
                                     (first buffers)))
      (change-to-buffer buffer))))

(defun enable-tabbar ()
  (setf (variable-value 'tabbar :global) t))
