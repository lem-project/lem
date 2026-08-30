(defpackage :lem-ansible-mode
  (:use :cl :lem :lem/language-mode :lem/language-mode-tools :lem-yaml-mode)
  (:export :*ansible-mode-hook*
           :*ansible-mode-keymap*
           :ansible-mode))
(in-package :lem-ansible-mode)


(define-major-mode ansible-mode yaml-mode
    (:name "Ansible"
     :description "Major mode for editing Ansible playbooks and tasks."
     :keymap *ansible-mode-keymap*
     :syntax-table lem-yaml-mode:*yaml-syntax-table*
     :mode-hook *ansible-mode-hook*)
  (setf (variable-value 'enable-syntax-highlight) t
        (variable-value 'indent-tabs-mode) nil
        (variable-value 'tab-width) 2
        (variable-value 'calc-indent-function) 'nil
        (variable-value 'line-comment) "#"))

(defun buffer-match-regex-n-p (buffer regex n)
  "Scan the first N lines of BUFFER to determine if any line matches REGEX."
  (with-point ((point (buffer-point buffer)))
    (buffer-start point)
    (loop
      :repeat n
      :do (when (ppcre:scan regex (line-string point))
            (return t))
      :while (line-offset point 1))))

(defun detect-ansible-file (buffer)
  "Detect if BUFFER contains Ansible syntax and change its major mode to `ansible-mode`."
  (when (and (eq (buffer-major-mode buffer) 'lem-yaml-mode:yaml-mode)
             (buffer-match-regex-n-p
              buffer
              "^\\s*(-\\s+(name|hosts|import_playbook|task|tasks)\\s*:|ansible\\.builtin\\.\\S*)"
              30))
    (change-buffer-mode buffer 'ansible-mode)))

;; hooks to detect file automatically
(add-hook *find-file-hook* 'detect-ansible-file)
(add-hook (variable-value 'before-save-hook :global) 'detect-ansible-file -1)