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

(defparameter *keyboard-table*
  (alexandria:plist-hash-table
   (list #\1 #\!
         #\2 #\"
         #\3 #\#
         #\4 #\$
         #\5 #\%
         #\6 #\&
         #\7 #\'
         #\8 #\(
         #\9 #\)
         #\- #\=
         #\^ #\~
         #\\ #\_
         #\@ #\`
         #\[ #\{
         #\; #\+
         #\: #\*
         #\] #\}
         #\, #\<
         #\. #\>
         #\/ #\?
         #\\ #\_)))

(defun convert-gesture-spec-data (data shiftp)
  (cond ((not shiftp)
         (values data nil))
        ((alpha-char-p (code-char data))
         (values (char-upcase (code-char data)) nil))
        (t
         (multiple-value-bind (value success)
             (gethash (code-char data) *keyboard-table*)
           (if success
               (values value nil)
               (values data shiftp))))))

(defun gesture-spec-to-key-for-windows (gesture-spec)
  (when (sys:gesture-spec-p gesture-spec)
    (let* ((data (sys:gesture-spec-data gesture-spec))
           (modifiers (sys:gesture-spec-modifiers gesture-spec))
           (shiftp (shift-bit-p modifiers))
           (ctrlp (control-bit-p modifiers))
           (metap (meta-bit-p modifiers)))
      (multiple-value-bind (data shiftp)
          (convert-gesture-spec-data data shiftp)
        (let ((sym (or (gethash data *data-sym-table*)
                       (typecase data
                         (string data)
                         (keyword (string-capitalize data))
                         (integer (string (code-char data)))
                         (character (string data))))))
          (when sym
            (cond ((and (not metap) ctrlp (not shiftp) (string= sym "i"))
                   (lem:make-key :sym "Tab"))
                  (t
                   (lem:make-key :meta metap
                                 :ctrl ctrlp
                                 :shift shiftp
                                 :sym sym)))))))))

(defun gesture-spec-to-key-for-linux (gesture-spec)
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

(defun gesture-spec-to-key (gesture-spec)
  #+windows
  (gesture-spec-to-key-for-windows gesture-spec)
  #-windows
  (gesture-spec-to-key-for-linux gesture-spec))
