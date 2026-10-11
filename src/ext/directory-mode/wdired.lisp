(uiop:define-package :lem/directory-mode/wdired
  (:use :cl
        :lem)
  (:import-from :lem/directory-mode/file
                :rename-file*)
  (:import-from :lem/directory-mode/internal
                :*default-sort-method*
                :get-name
                :get-pathname
                :update-buffer)
  (:import-from :lem/directory-mode/mode
                :directory-mode)
  (:export :wdired-mode
           :wdired-change-to-wdired-mode
           :wdired-finish-edit
           :wdired-abort-changes))
(in-package :lem/directory-mode/wdired)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:lock-package :lem/directory-mode/wdired))

(define-minor-mode wdired-mode
    (:name "Wdired"
     :description "Edit the file names of a directory buffer as text."
     :keymap *wdired-mode-keymap*
     :enable-hook 'wdired-mode-enable
     :disable-hook 'wdired-mode-disable))

;;; keybindings

;; Editing file names means typing has to insert text, so the printable
;; characters are bound to `self-insert' to shadow the directory-mode commands
;; they are bound to.  An undef-hook would do this in one step, but it also
;; hides every key it does not bind itself, such as C-x or M-x.
(loop :for code :from 33 :to 126
      :do (define-key *wdired-mode-keymap* (string (code-char code)) 'self-insert))
(define-key *wdired-mode-keymap* "Space" 'self-insert)

(define-key *wdired-mode-keymap* "C-c C-c" 'wdired-finish-edit)
(define-key *wdired-mode-keymap* "C-c C-k" 'wdired-abort-changes)
(define-key *wdired-mode-keymap* "C-x C-q" 'wdired-abort-changes)

;;; listing lines

(defun file-entry-p (point)
  "Whether the line POINT is on lists a file or a directory that can be renamed.
The parent directory entry `..' is listed but is never renamed, so it is not a
file entry here."
  (and (get-pathname point)
       (not (equal ".." (get-name point)))))

(defun line-name-region (line property name-start scan name-end)
  "Bound the file name of the file entry on the line LINE.
PROPERTY is the text property whose regions delimit the file name: `:file' in
the listing `wdired-change-to-wdired-mode' starts from, `:read-only' while the
names are editable.  Move NAME-START and NAME-END to the ends of the file name,
using SCAN as scratch space, and return T when the line has a file name."
  (move-point name-start line)
  (line-start name-start)
  (move-point name-end line)
  (line-end name-end)
  (when (next-single-property-change name-start property name-end)
    (move-point scan name-start)
    (when (next-single-property-change scan property name-end)
      (move-point name-end scan))
    t))

(defun line-file-region-p (line)
  "Whether the line LINE has a `:file' text property region in it."
  (with-point ((start line)
               (limit line))
    (line-start start)
    (line-end limit)
    ;; the scan would otherwise look past the end of an empty line
    (when (point< start limit)
      (next-single-property-change start :file limit))))

(defun count-file-entries (buffer)
  "Number of lines of BUFFER that list a file or directory that can be renamed."
  (with-point ((line (buffer-start-point buffer)))
    (let ((count 0))
      (loop
        (when (file-entry-p line) (incf count))
        (unless (line-offset line 1) (return count))))))

(defun make-region-read-only (start end)
  "Give the text between START and END a `:read-only' text property."
  (when (point< start end)
    (put-text-property start end :read-only t)))

(defun make-file-names-editable (buffer)
  "Make the file name of every file entry listed in BUFFER editable.
The rest of each line keeps a `:read-only' text property, so that only the
names of the entries can be edited."
  (remove-text-property (buffer-start-point buffer)
                        (buffer-end-point buffer)
                        :read-only)
  (with-point ((line (buffer-start-point buffer))
               (name-start (buffer-start-point buffer))
               (name-end (buffer-start-point buffer))
               (scan (buffer-start-point buffer)))
    (loop
      (cond ((file-entry-p line)
             (when (line-name-region line :file name-start scan name-end)
               ;; the mark column, the size, the date and the icon stay read-only,
               (move-point scan line)
               (line-start scan)
               (make-region-read-only scan name-start)
               ;; and so does the target of a symbolic link, if the line has one
               (move-point scan line)
               (line-end scan)
               (make-region-read-only name-end scan)))
            ((get-pathname line)
             ;; the parent directory `..' cannot be renamed: keep its line read-only
             (move-point scan line)
             (line-start scan)
             (move-point name-start line)
             (line-end name-start)
             (make-region-read-only scan name-start)))
      (unless (line-offset line 1) (return))))
  buffer)

(defun check-listing-unchanged (buffer entry-count)
  "Signal an editor error when the edited listing of BUFFER cannot be saved.
File names are renamed line by line, so a listing whose lines were added,
removed, merged or split has to be left with `wdired-abort-changes' instead of
being renamed only partially."
  (with-point ((line (buffer-start-point buffer)))
    (let ((count 0))
      (loop
        (cond ((file-entry-p line)
               (incf count))
              ((line-file-region-p line)
               (editor-error "A file name was split onto several lines; press C-c C-k to abort")))
        (unless (line-offset line 1) (return)))
      (unless (= count entry-count)
        (editor-error "The number of listed files changed; press C-c C-k to abort")))))

(defun collect-renames (buffer)
  "Return a (SOURCE . NEW-NAME) pair for every file name edited in BUFFER.
SOURCE is where the file is now, NEW-NAME the name the buffer shows."
  (with-point ((line (buffer-start-point buffer))
               (name-start (buffer-start-point buffer))
               (name-end (buffer-start-point buffer))
               (scan (buffer-start-point buffer)))
    (let ((renames '()))
      (loop
        (when (file-entry-p line)
          (if (line-name-region line :read-only name-start scan name-end)
              (let ((name (points-to-string name-start name-end)))
                (unless (string= name (get-name line))
                  (push (cons (get-pathname line) name) renames)))
              (editor-error "The file name on line ~D was deleted"
                            (line-number-at-point line))))
        (unless (line-offset line 1) (return)))
      (nreverse renames))))

;;; renaming

(defun check-edited-name (new-name)
  "Return NEW-NAME as a usable file name, or signal an editor error.
A trailing directory separator is removed because directories are listed with
one, and a name may not contain a line break."
  (let ((name (string-right-trim (string (uiop:directory-separator-for-host))
                                 new-name)))
    (when (alexandria:emptyp name)
      (editor-error "File name must not be empty"))
    (when (find-if (lambda (char) (member char '(#\Newline #\Return #\Null))) name)
      (editor-error "Invalid file name: ~S" new-name))
    name))

(defun rename-destination (source new-name directory)
  "Return the file NEW-NAME names in DIRECTORY, or NIL when SOURCE keeps its name.
Signal an editor error when NEW-NAME cannot be used."
  (let* ((new-name (check-edited-name new-name))
         (destination (merge-pathnames new-name directory)))
    (cond ((uiop:pathname-equal source destination)
           nil)
          ((probe-file destination)
           (editor-error "The filename already exists: ~A"
                         (uiop:native-namestring destination)))
          ((not (uiop:directory-exists-p (uiop:pathname-directory-pathname destination)))
           (editor-error "No such directory: ~A"
                         (uiop:native-namestring
                          (uiop:pathname-directory-pathname destination))))
          (t
           destination))))

(defun checked-renames (renames directory)
  "Return RENAMES, pairs of (SOURCE . NEW-NAME), as (SOURCE . DESTINATION) pairs.
Every name is checked before any file is renamed, so that an edit is either
applied completely or not at all."
  (let ((checked '()))
    (dolist (rename renames)
      (destructuring-bind (source . new-name) rename
        (alexandria:when-let ((destination (rename-destination source new-name directory)))
          (when (find destination checked :key #'cdr :test #'uiop:pathname-equal)
            (editor-error "~A is the new name of more than one file"
                          (uiop:native-namestring destination)))
          (push (cons source destination) checked))))
    (nreverse checked)))

(defun rename-file-buffer (source destination)
  "Rename the buffer visiting SOURCE, so that it visits DESTINATION."
  (alexandria:when-let ((buffer (get-file-buffer source)))
    (buffer-rename buffer (file-namestring destination))
    (setf (buffer-filename buffer) destination)))

(defun apply-renames (renames)
  "Rename the files in RENAMES, a list of (SOURCE . DESTINATION) pairs."
  (dolist (rename renames)
    (destructuring-bind (source . destination) rename
      (rename-file* source destination)
      (rename-file-buffer source destination))))

;;; minor mode

(defun wdired-mode-enable ()
  "Make the file names of the current directory buffer editable."
  (when (mode-active-p (current-buffer) 'directory-mode)
    (setf (buffer-read-only-p (current-buffer)) nil)
    (setf (buffer-value (current-buffer) :wdired-entry-count)
          (count-file-entries (current-buffer)))
    (make-file-names-editable (current-buffer))
    (message "Editing file names: C-c C-c saves, C-c C-k aborts")))

(defun wdired-mode-disable ()
  "List the current directory buffer read-only again."
  (when (mode-active-p (current-buffer) 'directory-mode)
    (setf (buffer-read-only-p (current-buffer)) t)
    (update-buffer (current-buffer)
                   :sort-method (or (buffer-value (current-buffer) :sort-method)
                                    *default-sort-method*)
                   :sort-reverse (buffer-value (current-buffer) :sort-reverse))))

;;; commands

(define-command wdired-change-to-wdired-mode () ()
  "Make the file names listed in this directory buffer editable.

  Editable names are saved with `wdired-finish-edit' (C-c C-c), which renames
  the files whose names were changed.  The parent directory entry `..' is not
  editable.

  `wdired-abort-changes' (C-c C-k or C-x C-q) discards the edited names and
  lists the directory read-only again."
  (unless (mode-active-p (current-buffer) 'directory-mode)
    (editor-error "This is not a directory buffer"))
  (enable-minor-mode 'wdired-mode))

(define-command wdired-finish-edit () ()
  "Rename the files whose names were edited, then list the directory as before.

  Only the file names that differ from the ones the listing showed are used,
  and the edit is rejected, leaving the buffer editable, when a new name cannot
  be used: an empty name, a name with a line break, a name that already exists,
  or a name in a directory that does not exist."
  (unless (mode-active-p (current-buffer) 'wdired-mode)
    (editor-error "The file names of this buffer are not editable"))
  (let* ((buffer (current-buffer))
         (directory (buffer-directory buffer)))
    (check-listing-unchanged buffer (buffer-value buffer :wdired-entry-count))
    (let ((renames (checked-renames (collect-renames buffer) directory)))
      (unwind-protect
           (when renames
             (apply-renames renames)
             (message "Renamed ~D file~:P" (length renames)))
        (wdired-abort-changes)))))

(define-command wdired-abort-changes () ()
  "Discard the edited file names and list the directory read-only again."
  (when (mode-active-p (current-buffer) 'wdired-mode)
    (disable-minor-mode 'wdired-mode)
    (message "File name changes discarded")))
