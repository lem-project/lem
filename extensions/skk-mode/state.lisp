(defpackage :lem-skk-mode/state
  (:use :cl :lem)
  (:export :skk-state
           :skk-input-mode
           :skk-preedit
           :skk-henkan-mode-p
           :skk-henkan-start
           :skk-henkan-key
           :skk-okurigana-consonant
           :skk-okurigana-kana
           :skk-candidates
           :skk-candidate-index
           :get-skk-state
           :clear-skk-state
           :reset-skk-state
           :with-skk-state))
(in-package :lem-skk-mode/state)

(deftype input-mode ()
  "SKK input mode type."
  '(member :hiragana :katakana :direct))

(defclass skk-state ()
  ((input-mode
    :initarg :input-mode
    :initform :hiragana
    :accessor skk-input-mode
    :type input-mode
    :documentation "Current input mode: :hiragana, :katakana, or :direct")
   (preedit
    :initarg :preedit
    :initform ""
    :accessor skk-preedit
    :type string
    :documentation "Romaji input buffer (preedit string)")
   (henkan-mode-p
    :initform nil
    :accessor skk-henkan-mode-p
    :type boolean
    :documentation "Non-nil when in conversion mode (after uppercase input)")
   (henkan-start
    :initform nil
    :accessor skk-henkan-start
    :documentation "Point marker for conversion start position")
   (henkan-key
    :initform ""
    :accessor skk-henkan-key
    :type string
    :documentation "Reading string for dictionary lookup")
   (okurigana-consonant
    :initform nil
    :accessor skk-okurigana-consonant
    :type (or null string)
    :documentation "Okurigana consonant for dictionary lookup (e.g., \"k\" for 書く)")
   (okurigana-kana
    :initform ""
    :accessor skk-okurigana-kana
    :type string
    :documentation "Okurigana kana part to append to candidate (e.g., \"く\")")
   (candidates
    :initform nil
    :accessor skk-candidates
    :type list
    :documentation "List of conversion candidates")
   (candidate-index
    :initform 0
    :accessor skk-candidate-index
    :type integer
    :documentation "Current candidate index (0-based)"))
  (:documentation "SKK conversion state for a buffer."))

(defun get-skk-state (&optional (buffer (current-buffer)))
  "Get or create SKK state for BUFFER."
  (or (buffer-value buffer 'skk-state)
      (setf (buffer-value buffer 'skk-state)
            (make-instance 'skk-state))))

(defun clear-skk-state (state)
  "Clear SKK state fields to initial values."
  (setf (skk-preedit state) ""
        (skk-henkan-mode-p state) nil
        (skk-henkan-key state) ""
        (skk-okurigana-consonant state) nil
        (skk-okurigana-kana state) ""
        (skk-candidates state) nil
        (skk-candidate-index state) 0)
  (when (skk-henkan-start state)
    (delete-point (skk-henkan-start state))
    (setf (skk-henkan-start state) nil))
  state)

(defun reset-skk-state (&optional (buffer (current-buffer)))
  "Reset SKK state to initial values for BUFFER."
  (clear-skk-state (get-skk-state buffer)))

(defmacro with-skk-state ((state-var &optional (buffer '(current-buffer))) &body body)
  "Execute BODY with STATE-VAR bound to SKK state of BUFFER."
  `(let ((,state-var (get-skk-state ,buffer)))
     ,@body))
