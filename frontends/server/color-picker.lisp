(defpackage :lem-server/color-picker
  (:use :cl :lem))
(in-package :lem-server/color-picker)

(defparameter *html*
  "
<label>
  <input id='picker' type='color'>
</label>

<code id='value'></code>

<script>
const input  = document.getElementById('picker');
const value  = document.getElementById('value');

function update(){
  value.textContent = input.value;
  invokeLem('color-picker/select', [input.value]);
}

input.addEventListener('input', update);

update();
</script>")

(defvar *callback* nil)
(defvar *color-picker-window* nil)

(defun delete-color-picker-window ()
  (remove-hook *editor-abort-hook* 'delete-color-picker-window)
  (when *color-picker-window* (delete-window *color-picker-window*)))

(defun make-color-picker-buffer ()
  (let ((buffer (make-buffer "*Color Picker*" :enable-undo-p nil)))
    (change-class buffer 'html-buffer :html *html*)
    buffer))

(defun make-color-picker-window ()
  (multiple-value-bind (x y) (lem-server/mouse:get-position)
    (delete-color-picker-window)
    (setf *color-picker-window*
          (make-floating-window :buffer (make-color-picker-buffer)
                                :x x
                                :y (1+ y)
                                :width 15
                                :height 2))
    (add-hook *editor-abort-hook*
              'delete-color-picker-window)))

(defmethod lem-color-preview:invoke-color-picker ((frontend lem-server::jsonrpc) callback)
  (setf *callback* callback)
  (make-color-picker-window))

(lem-server:register-method
 "color-picker/select"
 (lambda (args)
   (destructuring-bind (color) args
     (when *callback*
       (delete-color-picker-window)
       (funcall *callback* (parse-color color))))))
