(defpackage :lem-tests/visual-line
  (:use :cl :rove :lem)
  (:import-from
   :lem-tests/utilities
   :with-testing-buffer
   :make-text-buffer
   :lines))

(in-package :lem-tests/visual-line)

(defun point-at-line (buffer line-number)
  "temporary point at the start of LINE-NUMBER (0-based) of BUFFER."
  (let ((point (copy-point (buffer-start-point buffer) :temporary)))
    (line-offset point line-number)
    (line-start point)
    point))

(deftest visual-line-navigation-across-folds
  (lem-fake-interface:with-fake-interface ()
    (with-testing-buffer (buffer (make-text-buffer
                                  (lines "AAAA" "BBBB" "CCCC" "DDDD" "EEEE" "FFFF")))
      (labels ((fold-lines (first last)
                 "fold buffer lines FIRST..LAST (inclusive) into a single visual line."
                 (fold-region (point-at-line buffer first)
                              (point-at-line buffer (1+ last))))
               (expect-visual-line (containing-line first-line last-line)
                 (testing (format
                           nil
                           "buffer line ~D belongs to the visual line spanning lines ~D..~D"
                           containing-line
                           first-line
                           last-line)
                   (with-point ((p1 (point-at-line buffer containing-line))
                                (p2 (point-at-line buffer containing-line)))
                     (line-offset p1 0 2)
                     (line-offset p2 0 2)
                     (ok (= (position-at-point (visual-line-beginning p1))
                            (position-at-point (point-at-line buffer first-line))))
                     (ok (= (position-at-point (visual-line-end p2))
                            (position-at-point (line-end (point-at-line buffer last-line)))))))))
        (fold-lines 0 1)
        (fold-lines 3 4)
        ;; folded lines collapse with others, unfolded lines stand alone.
        (expect-visual-line 0 0 1)
        (expect-visual-line 1 0 1)
        (expect-visual-line 2 2 2)
        (expect-visual-line 3 3 4)
        (expect-visual-line 4 3 4)
        (expect-visual-line 5 5 5)))))