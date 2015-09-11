#|
  This code is a part of inquisitor project
  and a derivate from guess (https://github.com/zqwell/guess).
|#
;;; This code is derivative of libguess-1.0 and guess-0.1.0 for common lisp.
;;; 
;;; Copyright (c) 2011 zqwell <zqwell@gmail.com>
;;; 
;;; The following is the original copyright notice.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 
;;; 3. Neither the name of the authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this
;;;    software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; 
;;; Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
;;; 

(in-package :cl-user)
(defpackage inquisitor.encoding.dfa
  (:use :cl)
  (:import-from :inquisitor.encoding.keyword
                :enc-name->keyword)
  (:import-from :alexandria
                :with-gensyms)
  (:export :define-dfa
           :dfa-init
           :dfa-name
           :dfa-process
           :dfa-top
           :dfa-none

           :generate-order))
(in-package :inquisitor.encoding.dfa)



(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass <dfa> ()
    ((name   :initarg :name :accessor name-of)
     (states :initarg :states :accessor states-of)
     #+nil (instances :allocation :class :initform nil)))

  (defclass <state> ()
    ((name  :initarg :name  :accessor name-of)
     (index :initarg :index :accessor index-of)
     (arcs  :initarg :arcs  :accessor arcs-of :initform nil)))

  (defclass <arc> ()
    ((from-state :initarg :from-state  :accessor from-state-of)
     (to-state   :initarg :to-state    :accessor to-state-of)
     (ranges     :initarg :ranges      :accessor ranges-of)
     (index      :initarg :index       :accessor index-of)
     (score      :initarg :score       :accessor score-of)))

  (defun resolve-states (state-defs)
    (let ((states (mapcar (lambda (d i)
			    (make-instance '<state> :name (car d) :index i))
			  state-defs
			  (loop for i from 0 below (length state-defs) collect i))))
      (labels ((gen (s d i &aux (num-arcs (length (cdr d))))
		 (setf (arcs-of s)
		       (mapcar (lambda (arc aindex)
				 (make-instance '<arc>
						:from-state s
						:to-state (or (find-if (lambda (e)
									 (eq (name-of e) (cadr arc)))
								       states)
							      (error (format nil "no such state ~A" (cadr arc))))
						:ranges (car arc)
						:index aindex
						:score (caddr arc)))
			       (cdr d)
			       (loop repeat num-arcs for x from i collect x)))
		 (+ i num-arcs))
	       (fold (fun  state arg1 arg2)
		 (if (or (null arg1) (null arg2))
		     state
		     (fold fun
			   (funcall fun (car arg1) (car arg2) state)
			   (cdr arg1)
			   (cdr arg2)))))
	(fold #'gen 0 states state-defs)
	states)))

;;;;;; DFA

  (defmacro define-dfa (name &body states)
    (let ((name-st (intern (string-upcase (format nil "+~A-ST+" name))))
	  (name-ar (intern (string-upcase (format nil "+~A-AR+" name)))))
      `(unless (boundp ',name-st)
	 (let ((dfa (make-instance '<dfa> :name ',name :states (resolve-states ',states))))
	   (defconstant ,name-st (apply #'vector
					(loop for state in (states-of dfa)
					      collect (let ((vec (make-array 256 :initial-element -1)))
							(flet ((b2i (byte) (if (characterp byte) (char-code byte) byte)))
							  (dolist (br (arcs-of state))
							    (dolist (range (ranges-of br))
							      (if (consp range)
								  (fill vec (index-of br)
									:start (b2i (car range))
									:end   (+ (b2i (cadr range)) 1))
								  (setf (aref vec (b2i range)) (index-of br)))))
							  vec)))))
	   (defconstant ,name-ar (apply #'vector
					(loop for arc in (loop for state in (states-of dfa) appending (arcs-of state))
					      collect (cons (index-of (to-state-of arc)) (score-of arc)))))))))
  ) ;; eval-when



;;;; DFA Utility

(defmacro dfa-init (dfa-st dfa-ar dfa-name)
  `(vector ,dfa-st ,dfa-ar 0 1.0d0 ,dfa-name))

(defmacro dfa-name (dfa)   `(svref ,dfa 4))
(defmacro score (dfa)  `(svref ,dfa 3))
(defmacro state (dfa)  `(svref ,dfa 2))
(defmacro arcs (dfa)   `(svref ,dfa 1))
(defmacro states (dfa) `(svref ,dfa 0))

(defmacro dfa-alive (dfa) `(>= (the fixnum (state ,dfa)) (the fixnum 0)))

(defmacro dfa-next (dfa ch)
  `(when (dfa-alive ,dfa)
     (let ((temp (svref
		  (svref (states ,dfa) (state ,dfa))
		  ,ch)))
       (if (< (the fixnum temp) (the fixnum  0))
	   (setf (state ,dfa) -1)
	   (setf (state ,dfa) (the fixnum (car (svref (arcs ,dfa) temp)))
		 (score ,dfa) (* (the double-float (score ,dfa))
				 (the single-float (cdr (svref (arcs ,dfa) temp)))))))))

(defmacro dfa-process (order ch)
  (with-gensyms (gorder gch)
     `(let ((,gorder ,order)
	   (,gch ,ch))
	(or (loop for dfa in ,gorder
		  for i of-type fixnum from 0
		  do 
		     (when (dfa-alive dfa)
		       (when (dfa-alone dfa ,gorder)
			 (return (dfa-name dfa)))
		       (dfa-next (nth i ,gorder) ,gch)))
	    nil))))

(defun dfa-alone (dfa order)
  (unless (dfa-alive dfa)
    (return-from dfa-alone nil))
  (loop for d in order
	do (if (and (not (eql dfa d)) (dfa-alive d))
	       (return-from dfa-alone nil)))
  t)

(defun dfa-top (order)
  (let ((top nil))
    (loop for dfa in order do
      (if (and (dfa-alive dfa)
	       (or (null top)
		   (> (the double-float (score dfa)) (the double-float (score top)))))
	  (setf top dfa)))
    top))

(defun dfa-none (order)
  (dolist (d order)
    (if (dfa-alive d)
	(return-from dfa-none nil)))
  t)

(defmacro generate-order (&rest encodings)
  `(list
    ,@(mapcar (lambda (enc)
		(let ((dfa-st (find-symbol (string-upcase (format nil "+~A-ST+" (symbol-name enc))) :inquisitor.encoding.table))
		      (dfa-ar (find-symbol (string-upcase (format nil "+~A-AR+" (symbol-name enc))) :inquisitor.encoding.table))
		      (dfa-name (enc-name->keyword enc)))
		  `(dfa-init ,dfa-st ,dfa-ar ,dfa-name)))
		  encodings)))
