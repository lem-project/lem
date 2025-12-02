;;; Imported fuzzy-match.

(defpackage :lem/fuzzy-match
  (:use :cl)
  (:export :fuzzy-match
   :*threshold*)
  (:documentation "fuzzy-match a list of strings or a list of objects from an input string. Ignore dashes and spaces, give priority to objects starting with the search string."))

(in-package :lem/fuzzy-match)

(defvar *threshold* 0.1
  ;; devel: Learn good value.
  "The score threshold below which candidates are discarded.")

(defparameter *debug* nil
  "If t, print scores.")

;; yep we might use structures or objects or dicts, but so be it.
(defun item (plist)
  (getf plist :item))

(defun input (plist)
  (getf plist :input))

(defun score (plist)
  (getf plist :score))

(defun substring-norm (substrings string &key (substring-length 2))
  "Return the norm of SUBSTRINGS with regard to STRING.
The norm is closer to 1 if
- substrings start near the beginning of STRING;
- substrings length are closer to the length of STRING.

Only substrings of SUBSTRING-LENGTH characters or more are considered."
  ;; TODO: Remove duplicates in SUBSTRINGS?  Repeats could mean we insist more on it.
  (let ((position-factor 1.0)
        (length-factor 1.0)
        (long-substrings (remove-if (lambda (s) (> substring-length (length s)))
                                    substrings)))
    (if long-substrings
        (/ (apply #'+
                  (mapcar (lambda (s)
                            (let ((position (search s string)))
                              (if (not position)
                                  0
                                  (/ (+
                                      (* position-factor
                                         (/ 1
                                            ;; We use the sqrt to slow down the
                                            ;; decrease rate, we want the a
                                            ;; position of 10-15 still be >0.1.
                                            (sqrt (1+ position))))
                                      (* length-factor
                                         (/ (min (length s) (length string))
                                            (length string))))
                                     (+ position-factor length-factor)))))
                          long-substrings))
           (length long-substrings))
        0)))

(defun to-unicode (input)
  "Convert INPUT to (simple-array character) type."
  (if (typep input 'base-string)
      (coerce input `(simple-array character (,(length input))))
      input))

;; IDEA: Make score functions customizable, e.g. for global history.

(defun score-candidate (input candidate)
  "Return a CANDIDATE's score for INPUT.
A higher score means the candidate comes first."
  ;; The Jaccard metric seems to provide much better results than, say,
  ;; Damerau-Levensthein but it's much slower.
  (assert (stringp candidate))
  (+ (* 1.0 (mk-string-metrics:norm-damerau-levenshtein candidate input))
     (* 1.0 (substring-norm (str:split " " input) candidate))))

(defun score-sort-candidates (input pairs)
  "Score and sort PAIRS, the pair closest to INPUT in the levenshtein distance comes first.
PAIRS is a list of (search-string real-item)."
  ;; WARNING: mk-string-metrics works on low-level arrays and might not get
  ;; the text encoding right.  We need to make sure the candidates and the
  ;; input are of the same encoding.
  (let ((input (to-unicode input))
        (candidates (mapcar (lambda (elt)
                              (list :input (to-unicode (input elt))
                                    :item (item elt)))
                            pairs)))
    (flet ((score-pair (pair)
             (list :score (score-candidate input (input pair))
                   :input (input pair)
                   :item (item pair)))
           (sort-candidate (triplet1 triplet2)
             (> (getf triplet1 :score)
                (getf triplet2 :score))))
      (stable-sort (mapcar #'score-pair candidates)
                   #'sort-candidate))))

;; devel: add type declarations.
(defun find-exactly-matching-substrings (input candidates &key (substring-length 2))
  "Return the list of input substrings that match at least one candidate.
The substrings must be SUBSTRING-LENGTH characters long or more."
  (assert (stringp input))
  (assert (stringp (first candidates)))
  (let ((input-strings (delete-if (lambda (s) (< (length s) substring-length))
                                  (str:split " " input :omit-nulls t))))
    (when input-strings
      (delete-duplicates
       (loop for candidate in candidates
             append (remove-if
                     (lambda (i)
                       (not (search i candidate)))
                     input-strings))
       :test #'string=))))

(defun keep-exact-matches-in-candidates (input pairs)
  "Filter out non-exact matches from candidates.
If any input substring (split by whitespace) matches exactly (but not necessarily a whole word),
then all candidates that are not exactly matched by at least one substring are removed."
  (let ((exactly-matching-substrings (find-exactly-matching-substrings
                                      input
                                      (mapcar #'input pairs))))
    (if exactly-matching-substrings
        (remove-if (lambda (item)
                     (not (loop for i in exactly-matching-substrings
                                always (search i (input item)))))
                   pairs)
        pairs)))

(declaim (inline filter-by-threshold))
(defun filter-by-threshold (items &key (threshold *threshold*))
  "Keep items (plist with :string and :score keys) only if score is >= than THRESHOLD.
  If THRESHOLD isn't a number, return all items."
  (cond
    ((numberp threshold)
     (loop :for item :in items
           :if (>= (score item) threshold)
             :collect item))
    (t
     items)))

(defun fuzzy-match (input candidates &key (key #'identity) (threshold *threshold*))
  "From the user input and a list of candidates, return a filtered list of
candidates that have all the input words in them, and sort this list to have the
'most relevant' first.

KEY is a function to get the candidates' string representation. It will be funcall'ed.

THRESHOLD, a float between 0 and 1, is the minimal score for a matching result. If a match scores below it, it is discarded.

The match is case-sensitive if INPUT contains at least one uppercase character."
  ;; To sort by the display value, we store all the candidates in a
  ;; (display-value real-value) list or pairs.
  (unless key
    ;; when callers use :key nil
    (setf key #'identity))
  (let ((pairs (mapcar (lambda (elt)
                         (list :input (funcall key elt)
                               :item elt))
                       candidates)))

    ;; slight cleanup.
    (setf input (str:replace-all " " " " input))  ;; unbreakable whitespace

    ;; (setf input (str:replace-all " " "-" input))
    ;; (setf input (ppcre:regex-replace-all "-+" input "-" ))


    ;; Prefer to work with all downcased candidate strings,
    ;; except if the input contains one uppercase character.
    ;; DEVEL: have a parameter?
    (when (str:downcasep input)
      (setf pairs
            (mapcar (lambda (elt)
                      (setf (getf elt :input)
                            (string-downcase (getf elt :input)))
                      elt)
                    pairs)))

    (if (str:emptyp input)
         candidates
        (let* ((pairs (keep-exact-matches-in-candidates input pairs))
               ;; score and sort
               (score-items (score-sort-candidates input pairs))
               ;; filter low quality results
               ;; devel: have adaptive threshold on the result length?
               (score-items (filter-by-threshold score-items :threshold threshold)))
          (when *debug*
            (print (subseq score-items 0 10)))
          (mapcar #'item score-items)))))

#+(or)
;; test with objects, other than list of strings.
(progn
  (defstruct candidate
    (string)
    (stuff))

  (defparameter *objects*
    (list (make-candidate :string "project-switch")
          (make-candidate :string "bananas")))

  (equalp (first *objects*)
          (first
           (fuzzy-match "proj ws" *objects* :key #'candidate-string)))
  )
