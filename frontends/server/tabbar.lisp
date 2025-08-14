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
  (with-output-to-string (out)
    (format out "<head>")
    (write-line "<style>
/* ===== Theme tokens ===== */
:root {
  --bg: #1e1e1e;
  --bg-elev: #232323;
  --fg: #c8c8c8;
  --fg-dim: #9ca3af;
  --border: #2f2f2f;
  --accent: #4cc2ff; /* Zed-like cool color */
}

/* ===== Page base ===== */
html, body {
  margin: 0; padding: 0; height: 100%;
  background: var(--bg);
  color: var(--fg);
}

/* ===== Tabbar container ===== */
.lem-editor__tabbar {
  display: flex;
  align-items: flex-end;
  gap: 2px;
  padding: 0 8px;
  margin: 0;
  background:
    linear-gradient(180deg, #1c1c1c, var(--bg)) /* subtle top gradient */;
  border-bottom: 1px solid var(--border);
  overflow-x: auto;         /* horizontal scroll for multiple tabs */
  white-space: nowrap;      /* no line wrap */
  scrollbar-width: none;    /* Firefox: hide scrollbar */
  position: relative;
}
.lem-editor__tabbar::-webkit-scrollbar { display: none; } /* Chromium */

/* scroll shadows at edges */
.lem-editor__tabbar::before,
.lem-editor__tabbar::after {
  content: \"\";
  position: sticky;
  bottom: 0;
  width: 16px;
  height: 28px;
  pointer-events: none;
  z-index: 1;
}
.lem-editor__tabbar::before {
  left: 0;
  background: linear-gradient(90deg, var(--bg) 30%, transparent);
}
.lem-editor__tabbar::after {
  right: 0;
  background: linear-gradient(270deg, var(--bg) 30%, transparent);
}

/* ===== Tab button ===== */
.lem-editor__tabbar-button {
  position: relative;
  background: transparent;
  border: none;
  color: var(--fg-dim);
  padding: 6px 12px;
  border-radius: 9px 9px 0 0;     /* pill-like top rounded */
  font: 500 12.5px/1.35 ui-sans-serif, system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial;
  letter-spacing: .01em;
  cursor: pointer;
  transition:
    background .14s ease,
    color .14s ease,
    box-shadow .14s ease,
    transform .08s ease;
  transform: translateY(1px);     /* inactive tabs lowered by 1px */
}

/* hover: slightly raised */
.lem-editor__tabbar-button:hover {
  background: #262626;
  color: #e6e6e6;
  transform: translateY(0);
}

/* active: elevated surface + accent underline */
.lem-editor__tabbar-button.active {
  background: #3a3a3a; /* clearly brighter */
  color: #ffffff;
  font-weight: 600;
  box-shadow:
    inset 0 -2px 0 var(--accent),
    0 1px 2px rgba(0,0,0,0.4); /* subtle outer shadow for elevation */
  transform: translateY(0);
  z-index: 1;
}

/* focus ring (keyboard-friendly) */
.lem-editor__tabbar-button:focus-visible {
  outline: none;
  box-shadow:
    0 0 0 2px rgba(76,194,255,.35),
    inset 0 -2px 0 var(--accent);
}

/* close button × (pseudo-element, shown only on hover) */
.lem-editor__tabbar-button::after {
  content: '';
  font-size: 11px;
  opacity: 0;
  transition: opacity .12s ease;
  margin-left: 8px;
  vertical-align: middle;
}
.lem-editor__tabbar-button:hover::after,
.lem-editor__tabbar-button.active::after {
  opacity: .6;
}
.lem-editor__tabbar-button:active::after { opacity: .9; }

/* unsaved dot (just add data-dirty='true') */
.lem-editor__tabbar-button[data-dirty='true']::before {
  content: '';
  position: absolute;
  left: 6px; top: 9px;
  width: 6px; height: 6px;
  border-radius: 50%;
  background: var(--accent);
  filter: drop-shadow(0 0 2px rgba(76,194,255,.6));
}
/* reserve space for dot */
.lem-editor__tabbar-button[data-dirty='true'] { padding-left: 18px; }

/* disabled tab styling (if needed) */
.lem-editor__tabbar-button[disabled] {
  opacity: .45;
  cursor: default;
  pointer-events: none;
}

/* compact option (enable if needed)
.lem-editor__tabbar-button { padding: 4px 10px; font-size: 12px; }
*/
</style>
" out)
    (format out "</head>~%")
    (format out "<body>~%")
    (format out "<div class='lem-editor__tabbar'>~%")
    (loop :for buffer :in (get-tabbar-buffers)
          :do (if (eq buffer (current-buffer))
                  (format out
                          "<button class=\"lem-editor__tabbar-button active\">~A</button>~%"
                          (buffer-name buffer))
                  (format out
                          "<button class=\"lem-editor__tabbar-button\" onclick='handleClick(event)'>~A</button>~%"
                          (buffer-name buffer))))
    (format out "</div>~%")
    (write-line "
<script>
function handleClick(btn) {
  invokeLem('tabbar/select', btn.textContent);
}
const buttons = document.getElementsByTagName('button');
for (const button of buttons) {
  // Prevent focus movement (but allow clicks)
  button.addEventListener('mousedown', (e) => {
    const btn = e.target.closest('.lem-editor__tabbar-button');
    if (!btn) return;
    e.preventDefault();
  });
  // Also prevent focus movement in touch environments
  button.addEventListener('touchstart', (e) => {
    const btn = e.target.closest('.lem-editor__tabbar-button');
    if (!btn) return;
    e.preventDefault();
  }, { passive: false });
  // Click handling (keeping original focus)
  button.addEventListener('click', (e) => {
    const btn = e.target.closest('.lem-editor__tabbar-button');
    if (!btn) return;
    handleClick(btn);
  });
};
</script>"
                out)
    (format out "</body>~%")))

(lem-server:register-method
 "tabbar/select"
 (lambda (buffer-name)
   (send-event (lambda ()
                 (alexandria:when-let (buffer (get-buffer buffer-name))
                   (change-to-buffer buffer)
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
