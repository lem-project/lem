(in-package :lem-core)

(defparameter *default-popup-message-timeout* 5)

(defvar lem-core/popup-message-interface:*popup-messenger*)

(defgeneric lem-core/popup-message-interface:display-popup-message
    (popup-messenger
     buffer-or-string
     &key timeout
          destination-window
          source-window
          style))

(defgeneric lem-core/popup-message-interface:delete-popup-message (popup-messenger popup-message))

(defun display-popup-message (buffer-or-string
                              &key (timeout *default-popup-message-timeout*)
                                   destination-window
                                   source-window
                                   style)
  (lem-core/popup-message-interface:display-popup-message
   lem-core/popup-message-interface:*popup-messenger*
   buffer-or-string
   :timeout timeout
   :destination-window destination-window
   :source-window source-window
   :style style))

(defun delete-popup-message (popup-message)
  (lem-core/popup-message-interface:delete-popup-message
   lem-core/popup-message-interface:*popup-messenger*
   popup-message))

(defun display-popup-menu (items
                           &rest args
                           &key action-callback
                                print-spec
                                style
                                max-display-items)
  "Create a popup-menu and display it.

  By default, create it at the cursor position, but obey the :gravity of STYLE.

  Used for things such as completions.

  - `items`: a list of anything, but PRINT-SPEC must be able to turn it into a string.
  - `action-callback`: function taking an item (from `items`) as a parameter.  It is run once an item is selected.
  - `print-spec`: a function taking an `item` as a paremeter and returning its string representation.
  - style: a STYLE struct or a plist of args that is applied to MAKE-STYLE. Defaults to *STYLE*: uses borders and has no offset on the Y axis.

      Example: '(:use-border t :offset-y 0)

      Other properties include: :gravity (default: :cursor), :background-color, :offset-x, :cursor-invisible, :shape.

      Other gravity possibilities are (see `ensure-gravity`):

        :center
        :top-display
        :bottom-display
        :top
        :topright
        :cursor
        :follow-cursor
        :mouse-cursor
        :vertically-adjacent-window
        :vertically-adjacent-window-dynamic
        :horizontally-adjacent-window
        :horizontally-above-window

  - `max-display-items`: an integer limiting the number of items that can be displayed at once."
  (declare (ignore action-callback print-spec style max-display-items))
  (apply #'lem-if:display-popup-menu (implementation)
         items
         args))

(defun popup-menu-update (popup-menu items &rest args &key print-spec max-display-items keep-focus)
  (declare (ignore print-spec max-display-items keep-focus))
  (apply #'lem-if:popup-menu-update (implementation) popup-menu items args))

(defun popup-menu-quit (popup-menu)
  (lem-if:popup-menu-quit (implementation) popup-menu))

(defun popup-menu-down (popup-menu)
  (lem-if:popup-menu-down (implementation) popup-menu))

(defun popup-menu-up (popup-menu)
  (lem-if:popup-menu-up (implementation) popup-menu))

(defun popup-menu-first (popup-menu)
  (lem-if:popup-menu-first (implementation) popup-menu))

(defun popup-menu-last (popup-menu)
  (lem-if:popup-menu-last (implementation) popup-menu))

(defun popup-menu-select (popup-menu)
  (lem-if:popup-menu-select (implementation) popup-menu))
