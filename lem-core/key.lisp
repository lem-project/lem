(in-package :lem)

(export '(make-key
          key-p
          key-ctrl
          key-meta
          key-super
          key-hypher
          key-shift
          key-sym
          match-key
          insertion-key-sym-p
          key-to-char))

(defstruct (key (:constructor %make-key))
  (ctrl nil :type boolean)
  (meta nil :type boolean)
  (super nil :type boolean)
  (hypher nil :type boolean)
  (shift nil :type boolean)
  (sym 0 :type string))

(let ((table (make-hash-table :test 'equal)))
  (defun make-key (&rest args &key ctrl meta super hypher shift sym)
    (let ((hashkey (list ctrl meta super hypher shift sym)))
      (or (gethash hashkey table)
          (setf (gethash hashkey table)
                (apply #'%make-key args))))))

(defun match-key (key &key ctrl meta super hypher shift sym)
  (and (eq (key-ctrl key) ctrl)
       (eq (key-meta key) meta)
       (eq (key-super key) super)
       (eq (key-hypher key) hypher)
       (eq (key-shift key) shift)
       (equal (key-sym key) sym)))

(defun insertion-key-sym-p (sym)
  (= 1 (length sym)))

(defun key-to-char (key)
  (let ((char (cond ((string= (key-sym key) "Return")
                     #\Return)
                    ((string= (key-sym key) "Tab")
                     #\Tab)
                    ((= 1 (length (key-sym key)))
                     (char (key-sym key) 0)))))
    (when char
      (cond ((key-ctrl key)
             (let ((code (char-code (char-upcase char))))
               (cond ((<= 64 code 95)
                      (code-char (- code 64))))))
            (t char)))))
