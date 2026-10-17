(load ".qlot/setup.lisp")

(asdf:load-asd (merge-pathnames "yeditor/tx/yeditor-tx.asd" (uiop:getcwd)))
(asdf:load-system :yeditor-tx)

(defun run-batch (n)
  (let ((*tx-log* nil))
    (let ((start (get-internal-run-time)))
      (dotimes (i n)
        (install-effect :noop nil nil #identity))
      (let ((elapsed (/ (- (get-internal-run-time) start) internal-time-units-per-second)))
        (* elapsed 1e6))))) ; total µs

(defun report (values n)
  (let ((per-commit (mapcar (lambda (v) (/ v n)) values)))
    (let* ((sorted (sort (copy-list per-commit) #'<))
           (len (length sorted))
           (p50 (nth (floor (* 0.5 (1- len))) sorted))
           (p95 (nth (floor (* 0.95 (1- len))) sorted))
           (p99 (nth (floor (* 0.99 (1- len))) sorted)))
      (format t "p50=~6,2F µs, p95=~6,2F µs, p99=~6,2F µs~%" p50 p95 p99))))

(let ((iterations 10000))
  (let (results)
    (dotimes (rep 3)
      (format t "Repetition ~a~%" (1+ rep))
      (push (run-batch iterations) results))
    (report (nreverse results) iterations))
  (quit))