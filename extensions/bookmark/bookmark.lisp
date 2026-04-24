;;;; -*- mode: lisp; coding: utf-8 -*-
;;;; Bookmark package for lem
;;;;

(defpackage :lem-bookmark
  (:use :cl :lem)
  (:export
   ;; customization variables
   :*file*
   :*keymap*
   ;; bookmark type
   :bookmark
   :bookmark-name
   :bookmark-filename
   :bookmark-position
   :bookmark-p
   ;; internal functions for commands
   :load-from-file
   :save-to-file
   ;; commands
   :bookmark-load
   :bookmark-save
   :bookmark-set
   :bookmark-set-no-position
   :bookmark-set-no-overwrite
   :bookmark-set-no-position-no-overwrite
   :bookmark-delete
   :bookmark-delete-all
   :bookmark-rename
   :bookmark-relocate
   :bookmark-jump))

(in-package :lem-bookmark)

(setf (documentation *package* t)
      "Bookmarks for the lem editor.
Bookmarks are paths to files or directories that make it easy to open them.  Each
bookmark has a name and possibly a position associated with it.

The command BOOKMARK-SET is used to create a new bookmark, which points to the file
of the current buffer and the current cursor position.  The name of the bookmark will be
prompted for.  If you wish to not associate a position with the bookmark, you can use
BOOKMARK-SET-NO-POSITION.  This might be useful, if another package is managing file
positions for you.

To open a previously set bookmark, use BOOKMARK-JUMP.

The set bookmarks are not persisted automatically.  The commands BOOKMARK-SAVE and
BOOKMARK-LOAD are used to save and load the bookmarks from disk.  The variable
*FILE* configures from which file the bookmark information is read from/saved to.

The keymap *KEYMAP* has some pre-defined mappings for most of the available commands.

Use (DESCRIBE (FIND-PACKAGE \"LEM-BOOKMARK\")) to find all available commands.")

(defvar *file* #P"bookmarks.lisp-expr"
  "File in which bookmarks are saved.
If the file is a relative path, it is relative to LEM-HOME.")

(defvar *keymap*
  (make-keymap :description "Bookmark keymap")
  "Keymap for bookmark related commands.")

(defvar *bookmark-table* (make-hash-table :test #'equal))

(define-key *keymap* "x" 'bookmark-set)
(define-key *keymap* "X" 'bookmark-set-no-overwrite)
(define-key *keymap* "m" 'bookmark-set)
(define-key *keymap* "M" 'bookmark-set-no-overwrite)
(define-key *keymap* "j" 'bookmark-jump)
(define-key *keymap* "g" 'bookmark-jump)
(define-key *keymap* "l" 'bookmark-load)
(define-key *keymap* "s" 'bookmark-save)
(define-key *keymap* "d" 'bookmark-delete)
(define-key *keymap* "D" 'bookmark-delete-all)
(define-key *keymap* "r" 'bookmark-rename)
(define-key *keymap* "R" 'bookmark-rename-no-overwrite)
(define-key *keymap* "h" 'bookmark-relocate)

(defstruct bookmark
  (name)
  (filename)
  (position))

(defun bookmark-deserialize (list)
  (let* ((bookmark-name (car list))
         (bookmark-data (cdr list))
         (entry (make-bookmark
                 :name bookmark-name
                 :filename (cdr (assoc :filename bookmark-data))
                 :position (cdr (assoc :position bookmark-data)))))
    entry))

(defun bookmark-serialize (entry)
  (remove-if (lambda (field) (and (consp field) (null (cdr field))))
             (list (bookmark-name entry)
                   (cons :filename (bookmark-filename entry))
                   (cons :position (bookmark-position entry)))))

(defun %bookmark-insert (name buffer &key no-position)
  (let ((path (cond ((buffer-filename buffer)
                     (buffer-filename buffer))
                    ((string= "Directory" (mode-name (buffer-major-mode buffer)))
                     (buffer-directory buffer)))))
    (when path
      (setf (gethash name *bookmark-table*)
            (make-bookmark :name name
                           :filename path
                           :position (if no-position
                                         nil
                                         (position-at-point (buffer-point buffer)))))
      t)))

(defun %bookmark-find (name)
  (gethash name *bookmark-table*))

(defun %bookmark-delete (entry)
  (remhash (bookmark-name entry) *bookmark-table*))

(defun %bookmark-update (entry &key (new-name nil new-name-p)
                                    (new-filename nil new-filename-p)
                                    (new-position nil new-position-p))
  (when new-name-p
    (remhash (bookmark-name entry) *bookmark-table*)
    (setf (bookmark-name entry) new-name)
    (setf (gethash new-name *bookmark-table*) entry))
  (when new-filename-p
    (setf (bookmark-filename entry) new-filename))
  (when new-position-p
    (setf (bookmark-position entry) new-position)))

(defun %bookmark-relocate (entry buffer &key no-position)
  (%bookmark-update
   entry
   :new-filename (buffer-filename buffer)
   :new-position (if no-position
                     nil
                     (position-at-point (buffer-point buffer)))))

(defun %bookmark-apply-position (entry buffer)
  (when (bookmark-position entry)
    (move-to-position (buffer-point buffer) (bookmark-position entry))))

(defun prompt-for-bookmark (prompt)
  (let ((candidates (loop :for entry :being :the :hash-value :in *bookmark-table*
                          :collect (lem/completion-mode:make-completion-item
                                    :detail (if (bookmark-position entry)
                                                (format nil "~a:~a" (bookmark-filename entry) (bookmark-position entry))
                                                (format nil "~a" (bookmark-filename entry)))
                                    :label (bookmark-name entry)))))
    (prompt-for-string prompt
                       :completion-function (lambda (x) (completion-strings x candidates :key #'lem/completion-mode:completion-item-label))
                       :test-function (lambda (x) (find x candidates :test #'string= :key #'lem/completion-mode:completion-item-label))
                       :history-symbol 'prompt-for-bookmark)))

(defun load-from-file (file-path &optional (bookmark-table *bookmark-table*))
  (with-open-file (input file-path :direction :input)
    (loop :for bookmark-line :in (read input)
          :do (let ((bookmark-entry (bookmark-deserialize bookmark-line)))
                (setf (gethash (bookmark-name bookmark-entry) bookmark-table) bookmark-entry)))))

(define-command bookmark-load () ()
  "Load bookmarks from the file specified in the *FILE*."
  (let* ((file *file*)
         (full-path (if (uiop:relative-pathname-p file)
                        (uiop:merge-pathnames* file (lem-home))
                        file)))
    (handler-case (load-from-file full-path)
      (sb-int:simple-file-error (c)
        (editor-error "bookmark: ~a~&" c))))
  nil)

(defun save-to-file (file-path &optional (bookmark-table *bookmark-table*))
  (with-open-file (output file-path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write (loop :for entry :being :the :hash-value :in bookmark-table
                 :collect (bookmark-serialize entry))
           :stream output)
    (write-line "" output))
  nil)

(define-command bookmark-save () ()
  "Save bookmarks to *FILE*."
  (let* ((file *file*)
         (full-path (if (uiop:relative-pathname-p file)
                        (uiop:merge-pathnames* file (lem-home))
                        file)))
    (handler-case (save-to-file full-path)
      (sb-int:simple-file-error (c)
        (editor-error "bookmark: ~a~&" c)))))

(define-command bookmark-set (name)
  ((prompt-for-string "Boorkmark name: " :initial-value ""))
  "Set a new bookmark with NAME for the current buffer.
If a bookmark with NAME already exists, it will be overwritten.

If called interactively, prompt for NAME."
  (unless (%bookmark-insert name (current-buffer))
    (editor-error "bookmark: Buffer not visiting a file or directory~&")))

(define-command bookmark-set-no-position (name)
  ((prompt-for-string "Boorkmark name: " :initial-value ""))
  "Set a new bookmark with NAME for the current buffer without position.
If a bookmark with NAME already exists, it will be overwritten.

If called interactively, prompt for NAME."
  (unless (%bookmark-insert name (current-buffer) :no-position t)
    (editor-error "bookmark: Buffer not visiting a file or directory~&")))

(define-command bookmark-set-no-overwrite (name)
  ((prompt-for-string "Boorkmark name: " :initial-value ""))
  "Set a new bookmark with NAME for the current buffer.
If a bookmark with NAME already exists, it will be left unchanged.

If called interactively, prompt for NAME."
  (if (gethash name *bookmark-table*)
      (editor-error "bookmark: ~a: Bookmark already exists~&" name)
      (bookmark-set name)))

(define-command bookmark-set-no-position-no-overwrite (name)
  ((prompt-for-string "Boorkmark name: " :initial-value ""))
  "Set a new bookmark with NAME for the current buffer without position.
If a bookmark with NAME already exists, it will be left unchanged.

If called interactively, prompt for NAME."
  (if (gethash name *bookmark-table*)
      (editor-error "bookmark: ~a: Bookmark already exists~&" name)
      (bookmark-set-no-position name)))

(define-command bookmark-delete (name) ((prompt-for-bookmark "Delete bookmark: "))
  "Delete the bookmark with NAME.

If called interactively, prompt for NAME."
  (if (null (gethash name *bookmark-table*))
      (editor-error "bookmark: ~a: Bookmark does not exist~&" name)
      (%bookmark-delete (gethash name *bookmark-table*))))

(define-command bookmark-delete-all () ()
  "Delete all bookmarks."
  (if (<= (hash-table-count *bookmark-table*) 0)
      (editor-error "bookmark: No bookmarks available~&")
    (when (prompt-for-y-or-n-p (format nil "Do you really want to delete ~a bookmark~a?"
                                       (hash-table-count *bookmark-table*)
                                       (if (< 1 (hash-table-count *bookmark-table*)) "s" "")))
      (setq *bookmark-table* (clrhash *bookmark-table*)))))

(define-command bookmark-rename (old-name new-name) ((prompt-for-bookmark "Rename bookmark: ")
                                                     (prompt-for-string "New bookmark name: "))
  "Rename the bookmark with OLD-NAME to NEW-NAME.
If a bookmark with NEW-NAME already exists, it will be overwritten.

If called interactively, prompt for OLD-NAME and NEW-NAME."
  (let ((entry (gethash old-name *bookmark-table*)))
    (if (null entry)
        (editor-error "bookmark: ~a: Bookmark does not exist~&" old-name)
        (%bookmark-update entry :new-name new-name))))

(define-command bookmark-rename-no-overwrite (old-name new-name) ((prompt-for-bookmark "Rename bookmark: ")
                                                                  (prompt-for-string "New bookmark name: "))
  "Rename the bookmark with OLD-NAME to NEW-NAME.
If a bookmark with NEW-NAME already exists, it will be left unchanged.

If called interactively, prompt for OLD-NAME and NEW-NAME."
  (let ((entry (gethash old-name *bookmark-table*)))
    (if (null entry)
        (editor-error "bookmark: ~a: Bookmark does not exist~&" old-name)
        (if (not (null (gethash new-name *bookmark-table*)))
            (editor-error "bookmark: ~a Bookmark already exists~&" new-name)
            (%bookmark-update entry :new-name new-name)))))

(define-command bookmark-relocate (name) ((prompt-for-bookmark "Relocate bookmark: "))
"Relocate the bookmark NAME to the position and file of the current buffer.

If called interactively, prompt for NAME."
(let ((entry (gethash name *bookmark-table*)))
  (if (null entry)
      (editor-error "bookmark: ~a: Bookmark does not exist~&" name)
      (let ((buffer (current-buffer)))
        (if (null (buffer-filename buffer))
            (editor-error "bookmark: Buffer not visiting a file or directory~&")
            (%bookmark-relocate entry buffer))))))

(define-command bookmark-jump (name) ((prompt-for-bookmark "Jump to bookmark: "))
"Jump to the bookmark with NAME in the current window.
If the bookmark is associated with a position, jump to it.

If called interactively, prompt for NAME."
(let ((entry (gethash name *bookmark-table*)))
  (if (null entry)
      (editor-error "bookmark: ~a: Bookmark does not exist~&" name)
      (let ((buffer (find-file (bookmark-filename entry))))
        (%bookmark-apply-position entry buffer)))))
