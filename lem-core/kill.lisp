(in-package :lem)

(export '(*kill-ring-max*
          *kill-before-p*
          kill-append
          kill-push
          kill-ring-rotate
          kill-ring-first-string
          kill-ring-nth-string
          kill-ring-new))

(defvar *kill-ring* nil)
(defvar *kill-ring-yank-ptr* nil)
(defvar *kill-ring-max* 10)

(defvar *kill-new-flag* t)
(defvar *kill-before-p* nil)

(defun kill-append (string before-p)
  (setf (car *kill-ring*)
        (if before-p
            (concatenate #+lispworks 'lw:simple-bmp-string #-lispworks 'string
                         string
                         (car *kill-ring*))
            (concatenate #+lispworks 'lw:simple-bmp-string #-lispworks 'string
                         (car *kill-ring*)
                         string))))

(defun kill-push (string)
  (cond
    (*kill-new-flag*
     (push string *kill-ring*)
     (when (nthcdr *kill-ring-max* *kill-ring*)
       (setq *kill-ring*
             (subseq *kill-ring* 0 *kill-ring-max*)))
     (setq *kill-ring-yank-ptr* *kill-ring*)
     (setq *kill-new-flag* nil)
     (clipboard-set string))
    (t
     (clipboard-set (kill-append string *kill-before-p*))))
  t)

(defun current-kill-ring ()
  (clipboard-get
   (lambda (string)
     (if (and string (string/= string ""))
         string
         (kill-ring-nth 1)))))

(defun kill-ring-nth (n)
  (do ((ptr *kill-ring-yank-ptr*
            (or (cdr ptr)
                *kill-ring*))
       (n n (1- n)))
      ((>= 1 n)
       (car ptr))))

(defun kill-ring-rotate ()
  (setf *kill-ring-yank-ptr*
        (or (cdr *kill-ring-yank-ptr*)
            *kill-ring*)))

(defun kill-ring-first-string ()
  (car *kill-ring-yank-ptr*))

(defun kill-ring-nth-string (n)
  (kill-ring-nth n))

(defun kill-ring-new ()
  (setf *kill-new-flag* t))

(let* ((q)
       (thread)
       (initval (gensym))
       (result initval))
  (defun run-clipboard-thread ()
    (setf q (make-event-queue))
    (setf thread
          (bt:make-thread (lambda ()
                            (loop :for arg := (dequeue-event nil q)
                                  :do (if (functionp arg)
                                          (setf result
                                                (funcall arg (trivial-clipboard:text)))
                                          (ignore-errors (trivial-clipboard:text arg))))))))

  (defun clipboard-set (arg)
    (unless thread (run-clipboard-thread))
    (ignore-errors
     (send-event arg q)))

  (defun clipboard-get (fn)
    (unless thread (run-clipboard-thread))
    (setf result initval)
    (send-event fn q)
    (loop :while (eq result initval))
    result))
