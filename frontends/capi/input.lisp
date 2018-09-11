(in-package :lem-capi)

(defun shift-bit-p (modifiers)
  (/= 0 (logand modifiers sys:gesture-spec-shift-bit)))

(defun control-bit-p (modifiers)
  (/= 0 (logand modifiers sys:gesture-spec-control-bit)))

(defun meta-bit-p (modifiers)
  (/= 0 (logand modifiers sys:gesture-spec-meta-bit)))

(defparameter *data-sym-table*
  (alexandria:alist-hash-table
   `((,(char-code #\Return) . "Return")
     (,(char-code #\Tab) . "Tab")
     (,(char-code #\Escape) . "Escape")
     (,(char-code #\Backspace) . "Backspace")
     (,(char-code #\Space) . "Space")
     (:next . "PageDown")
     (:prior . "PageUp"))))

(defun gesture-spec-to-key (gesture-spec)
  (when (sys:gesture-spec-p gesture-spec)
    (let* ((data (sys:gesture-spec-data gesture-spec))
           (modifiers (sys:gesture-spec-modifiers gesture-spec))
           (shiftp (shift-bit-p modifiers))
           (ctrlp (control-bit-p modifiers))
           (metap (meta-bit-p modifiers))
           (sym (or (gethash data *data-sym-table*)
                    (typecase data
                      (string data)
                      (keyword (string-capitalize data))
                      (integer (string (code-char data)))))))
      (when sym
        (cond ((and (not metap) ctrlp (not shiftp) (string= sym "i"))
               (lem:make-key :sym "Tab"))
              (t
               (lem:make-key :meta metap
                             :ctrl ctrlp
                             :shift shiftp
                             :sym sym)))))))
