(in-package :lem-mcp-server)

;;; Display/Screen Operation Tools

;;; Helper functions

(defun window-id (window)
  "Generate a unique ID for a window."
  (format nil "window-~A" (sxhash window)))

(defun get-write-state (buffer)
  "Get the write state of a buffer."
  (cond ((buffer-read-only-p buffer) "readonly")
        ((buffer-modified-p buffer) "modified")
        (t "unmodified")))

(defun get-scroll-percent (window)
  "Get the scroll position as a percentage string."
  (let* ((buffer (window-buffer window))
         (total-lines (buffer-nlines buffer))
         (view-point (window-view-point window))
         (top-line (line-number-at-point view-point))
         (height (window-height window)))
    (cond ((<= total-lines height) "All")
          ((= top-line 1) "Top")
          ((>= (+ top-line height) total-lines) "Bot")
          (t (format nil "~D%" (round (* 100 (/ (1- top-line) (max 1 (- total-lines height))))))))))

(defun get-region-info (window)
  "Get selection/region information for a window."
  (let* ((buffer (window-buffer window))
         (mark (buffer-mark buffer)))
    (if (and mark (mark-active-p buffer))
        (let ((point (buffer-point buffer)))
          `(("active" . t)
            ("start" . (("line" . ,(line-number-at-point mark))
                        ("column" . ,(point-charpos mark))))
            ("end" . (("line" . ,(line-number-at-point point))
                      ("column" . ,(point-charpos point))))))
        `(("active" . :false)))))

(defun color-to-string (color)
  "Convert a color to a string representation."
  (typecase color
    (null :null)
    (string color)
    (t (format nil "~A" color))))

(defun attribute-to-json (attr)
  "Convert a Lem attribute to JSON-serializable form."
  (if attr
      `(("foreground" . ,(color-to-string (attribute-foreground attr)))
        ("background" . ,(color-to-string (attribute-background attr)))
        ("bold" . ,(if (attribute-bold attr) t :false))
        ("underline" . ,(if (attribute-underline attr) t :false))
        ("reverse" . ,(if (attribute-reverse attr) t :false)))
      `(("foreground" . :null)
        ("background" . :null)
        ("bold" . :false)
        ("underline" . :false)
        ("reverse" . :false))))

(defun get-line-attributes (point)
  "Get attributes for a line at point. Returns list of (start end attribute) tuples."
  (handler-case
      (let ((attrs (lem-core::get-string-and-attributes-at-point point)))
        (cdr attrs))
    (error () nil)))

(defun get-visible-lines (window)
  "Get visible lines with their content and attributes."
  (let ((result '()))
    (with-point ((point (window-view-point window)))
      (loop :for y :from 0 :below (window-height window)
            :while (not (end-buffer-p point))
            :do (let ((line-num (line-number-at-point point))
                      (text (line-string point))
                      (attrs (get-line-attributes point)))
                  (push `(("line_number" . ,line-num)
                          ("text" . ,text)
                          ("wrapped" . :false)
                          ("attributes" . ,(if attrs
                                               (mapcar (lambda (attr-spec)
                                                         (destructuring-bind (start end attr &rest rest) attr-spec
                                                           (declare (ignore rest))
                                                           `(("start" . ,start)
                                                             ("end" . ,end)
                                                             ("style" . ,(attribute-to-json attr)))))
                                                       attrs)
                                               ())))
                        result))
                (unless (line-offset point 1)
                  (return))))
    (nreverse result)))

(defun get-modeline-info (window)
  "Get modeline information for a window."
  (when (window-use-modeline-p window)
    (let ((buffer (window-buffer window)))
      `(("buffer_name" . ,(buffer-name buffer))
        ("write_state" . ,(get-write-state buffer))
        ("major_mode" . ,(mode-name (buffer-major-mode buffer)))
        ("minor_modes" . ,(mapcar (lambda (mode) (mode-name mode))
                                  (buffer-minor-modes buffer)))
        ("position" . ,(format nil "~D:~D"
                               (line-number-at-point (buffer-point buffer))
                               (point-charpos (buffer-point buffer))))
        ("scroll" . ,(get-scroll-percent window))))))

(defun get-window-info (window)
  "Get comprehensive information about a window."
  (let* ((buffer (window-buffer window))
         (point (buffer-point buffer))
         (is-current (eq window (current-window))))
    `(("id" . ,(window-id window))
      ("type" . ,(cond ((floating-window-p window) "floating")
                       ((header-window-p window) "header")
                       (t "normal")))
      ("active" . ,(if is-current t :false))
      ("position" . (("x" . ,(window-x window))
                     ("y" . ,(window-y window))))
      ("size" . (("width" . ,(window-width window))
                 ("height" . ,(window-height window))))
      ("buffer" . (("name" . ,(buffer-name buffer))
                   ("filename" . ,(or (buffer-filename buffer) :null))
                   ("modified" . ,(if (buffer-modified-p buffer) t :false))
                   ("readonly" . ,(if (buffer-read-only-p buffer) t :false))))
      ("cursor" . (("line" . ,(line-number-at-point point))
                   ("column" . ,(point-charpos point))
                   ("screen_x" . ,(window-cursor-x window))
                   ("screen_y" . ,(window-cursor-y window))))
      ("region" . ,(get-region-info window))
      ("view" . (("top_line" . ,(line-number-at-point (window-view-point window)))
                 ("visible_lines" . ,(window-height window))
                 ("scroll_percent" . ,(get-scroll-percent window))))
      ("visible_content" . ,(get-visible-lines window))
      ("modeline" . ,(or (get-modeline-info window) :null)))))

(defun get-prompt-window-info ()
  "Get prompt window (minibuffer equivalent) state."
  (let ((prompt-window (active-prompt-window)))
    (if prompt-window
        (let ((buffer (window-buffer prompt-window)))
          `(("active" . ,(if (eq prompt-window (current-window)) t :false))
            ("prompt" . :null)
            ("input" . ,(buffer-text buffer))
            ("message" . :null)))
        `(("active" . :false)
          ("prompt" . :null)
          ("input" . "")
          ("message" . :null)))))

(defun get-overlay-type (overlay)
  "Determine the type of an overlay."
  (typecase overlay
    (lem-core::cursor-overlay "cursor")
    (lem-core::line-overlay "line")
    (lem-core::line-endings-overlay "line-endings")
    (t "highlight")))

(defun get-overlay-info (overlay)
  "Get information about a single overlay."
  (let ((start (overlay-start overlay))
        (end (overlay-end overlay)))
    `(("buffer" . ,(buffer-name (overlay-buffer overlay)))
      ("type" . ,(get-overlay-type overlay))
      ("start" . (("line" . ,(line-number-at-point start))
                  ("column" . ,(point-charpos start))))
      ("end" . (("line" . ,(line-number-at-point end))
                ("column" . ,(point-charpos end))))
      ("attribute" . ,(attribute-to-json (overlay-attribute overlay)))
      ("temporary" . ,(if (lem-core::overlay-temporary-p overlay) t :false)))))

(defun get-all-overlays ()
  "Get all overlays from all buffers."
  (let ((result '()))
    (dolist (buffer (buffer-list))
      (dolist (overlay (lem-core::buffer-overlays buffer))
        (push (get-overlay-info overlay) result)))
    (nreverse result)))

(defun get-header-windows-info ()
  "Get information about header windows."
  (let ((result '()))
    (dolist (window (frame-header-windows (current-frame)))
      (push `(("id" . ,(window-id window))
              ("position" . (("x" . ,(window-x window))
                             ("y" . ,(window-y window))))
              ("size" . (("width" . ,(window-width window))
                         ("height" . ,(window-height window))))
              ("content" . ,(buffer-text (window-buffer window))))
            result))
    (nreverse result)))

;;; Main tool definition

(define-mcp-tool "editor_get_screen" (include-floating-windows)
  (:description "Get current screen state including all windows, their content, cursor positions, overlays, and UI elements. Returns complete visual state of the editor."
   :input-schema (("type" . "object")
                  ("properties" . (("include_floating_windows" . (("type" . "boolean")
                                                                   ("description" . "Include floating windows (popups, prompts). Default: true")))))
                  ("required" . ())))
  (let* ((include-floating (if (null include-floating-windows) t include-floating-windows))
         (all-windows (window-list))
         (floating-windows (when include-floating
                             (frame-floating-windows (current-frame))))
         (prompt-window (active-prompt-window))
         (windows-to-process (remove-if (lambda (w)
                                          (or (header-window-p w)
                                              (eq w prompt-window)))
                                        all-windows)))
    (with-output-to-string (s)
      (yason:encode
       (alist-to-hash-table
        `(("display" . (("width" . ,(display-width))
                        ("height" . ,(display-height))))
          ("current_window_id" . ,(window-id (current-window)))
          ("windows" . ,(mapcar #'get-window-info windows-to-process))
          ("floating_windows" . ,(when include-floating
                                   (mapcar #'get-window-info floating-windows)))
          ("header_windows" . ,(get-header-windows-info))
          ("prompt" . ,(get-prompt-window-info))
          ("overlays" . ,(get-all-overlays))))
       s))))
