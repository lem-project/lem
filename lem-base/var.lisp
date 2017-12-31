(in-package :lem-base)

(annot:enable-annot-syntax)

(export '(editor-variable
          define-editor-variable
          clear-editor-local-variables
          variable-value
          variable-documentation
          find-editor-variable))

(defvar *editor-variables* '())

(defstruct editor-variable
  value
  documentation
  local-indicator
  change-value-hook)

(setf (documentation 'editor-variable 'type)
      @lang(:jp "`editor-variable`はエディタ内で使われる変数です。  
バッファローカルな変数や大域的な値を管理するために使います。"))

(defmacro define-editor-variable (var &optional value documentation change-value-hook)
  @lang(:jp "エディタ変数`var`を定義します。  
`value`はそのエディタ変数に束縛されている大域的な値です。  
`documentation`はそのエディタ変数の文書文字列です。  
`change-value-hook`はそのエディタ変数の大域的な値が変更されるときに呼び出されるフック関数です。
")
  (check-type var symbol)
  `(unless (get ',var 'editor-variable)
     (defvar ,var)
     (pushnew ',var *editor-variables*)
     (setf (get ',var 'editor-variable)
           (make-editor-variable :value ,value
                                 :documentation ,documentation
                                 :local-indicator (gensym ,(string var))
                                 :change-value-hook ,change-value-hook))
     t))

(defun clear-editor-local-variables (buffer)
  @lang(:jp "`buffer`の全てのバッファローカルなエディタ変数を未束縛にします。")
  (dolist (symbol *editor-variables*)
    (buffer-unbound buffer
                    (editor-variable-local-indicator
                     (get symbol 'editor-variable)))))

(defun editor-variable-error (symbol)
  (error "~A is not editor variable" symbol))

(defun check-editor-variable (symbol)
  (unless (editor-variable-p (get symbol 'editor-variable))
    (editor-variable-error symbol)))

(defun ensure-buffer (where)
  (if (pointp where)
      (point-buffer where)
      (progn
        (check-type where buffer)
        where)))

(defun variable-value (symbol &optional (kind :default) (where nil wherep))
  @lang(:jp "`symbol`のエディタ変数の値を返します。  
`where`はバッファです、未指定なら`current-buffer`になります。  
`kind`が`:default`の場合は`where`のバッファローカルなエディタ変数が束縛されていればそれを返し、  
無ければ大域的なエディタ変数の値を返します。  
`kind`が`:buffer`の場合は`where`のバッファローカルなエディタ変数が束縛されていればそれを返し、
無ければNILを返します。  
`kind`が`:global`の場合は大域的なエディタ変数を返します。
")
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (ecase kind
      ((:default)
       (let* ((buffer (if wherep
                          (ensure-buffer where)
                          (current-buffer)))
              (default '#:default)
              (value (buffer-value buffer
                                   (editor-variable-local-indicator var)
                                   default)))
         (if (eq value default)
             (editor-variable-value var)
             value)))
      ((:buffer)
       (let ((buffer (if wherep
                         (ensure-buffer where)
                         (current-buffer))))
         (buffer-value buffer
                       (editor-variable-local-indicator var))))
      ((:global)
       (editor-variable-value var)))))

(defun (setf variable-value) (value symbol &optional (kind :default) (where nil wherep))
  @lang(:jp "`symbol`のエディタ変数の値に`value`を束縛します。  
`where`はバッファです、未指定なら`current-buffer`になります。  
`kind`が`default`か`buffer`の場合は、`where`のバッファローカルなエディタ変数に`value`を束縛します。  
`kind`が`global`の場合は、大域的なエディタ変数に`value`を束縛します。  
エディタ変数に`change-value-hook`があれば値を束縛する前にその関数が`value`を引数にして呼び出されます。  
")
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (ecase kind
      ((:default :buffer)
       (let ((buffer (if wherep
                         (ensure-buffer where)
                         (current-buffer))))
         (setf (buffer-value buffer
                             (editor-variable-local-indicator var))
               value)))
      ((:global)
       (let ((fn (editor-variable-change-value-hook var)))
         (when fn
           (funcall fn value)))
       (setf (editor-variable-value var) value)))))

(defun variable-documentation (symbol)
  @lang(:jp "エディタ変数`symbol`の文書文字列を返します。")
  (let ((var (get symbol 'editor-variable)))
    (unless (editor-variable-p var)
      (editor-variable-error symbol))
    (editor-variable-documentation var)))

(defun find-editor-variable (var)
  (find var *editor-variables* :test 'string-equal))
