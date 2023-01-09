;;; swank-sbcl-exts.lisp --- Misc extensions for SBCL
;;
;; Authors: Tobias C. Rittweiler <tcr@freebits.de>
;;
;; License: Public Domain
;;

(in-package :micros)

;; We need to do this so users can place `slime-sbcl-exts' into their
;; ~/.emacs, and still use any implementation they want.
#+sbcl
(progn
  
;;; Display arglist of instructions.
;;;
(defmethod compute-enriched-decoded-arglist ((operator-form (eql 'sb-assem:inst))
                                             argument-forms)
  (flet ((decode-instruction-arglist (instr-name instr-arglist)
           (let ((decoded-arglist (decode-arglist instr-arglist)))
             ;; The arglist of INST is (instruction ...INSTR-ARGLIST...).
             (push 'sb-assem::instruction (arglist.required-args decoded-arglist))
             (values decoded-arglist
                     (list (string-downcase instr-name))
                     t))))
    (if (null argument-forms)
        (call-next-method)
        (destructuring-bind (instruction &rest args) argument-forms
          (declare (ignore args))
          (let* ((instr-name
                   (typecase instruction
                     (arglist-dummy
                      (string-upcase (arglist-dummy.string-representation instruction)))
                     (symbol
                      (string-upcase instruction))))
                 (instr-fn
                   #+(and
                      #.(micros/backend:with-symbol '*inst-encoder* 'sb-assem)
                      #.(micros/backend:with-symbol '*backend-instruction-set-package* 'sb-assem))
                   (or (gethash (find-symbol instr-name sb-assem::*backend-instruction-set-package*)
                                sb-assem::*inst-encoder*)
                       (find-symbol (format nil "M:~A" instr-name)
                                    sb-assem::*backend-instruction-set-package*))))
            (when (consp instr-fn)
              (setf instr-fn (car instr-fn)))
            (cond ((functionp instr-fn)
                   (with-available-arglist (arglist) (arglist instr-fn)
                     (decode-instruction-arglist instr-name (cdr arglist))))
                  ((fboundp instr-fn)
                   (with-available-arglist (arglist) (arglist instr-fn)
                     ;; SB-ASSEM:INST invokes a symbolic INSTR-FN with
                     ;; current segment and current vop implicitly.
                     (decode-instruction-arglist instr-name
                                                 (if (or (get instr-fn :macro)
                                                         (macro-function instr-fn))
                                                     arglist
                                                     (cdr arglist)))))
                  (t
                   (call-next-method))))))))


) ; PROGN
