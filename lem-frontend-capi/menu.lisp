(in-package :lem-capi)

(defclass menu (lem.menu-mode:menu)
  ((panel
    :initarg :panel
    :accessor menu-panel)))

(defmethod lem-if:display-menu ((implementation capi-impl) menu name)
  (let ((multi-column-list-panel
          (make-instance 'capi:multi-column-list-panel
                         :filter t
                         :columns (mapcar (lambda (column)
                                            `(:title ,column))
                                          (lem.menu-mode::menu-columns menu))
                         :items (lem.menu-mode::menu-items menu)
                         :column-function (lem.menu-mode::menu-column-function menu)
                         :callback-type :data
                         :action-callback (lambda (item)
                                            (funcall (lem.menu-mode::menu-select-callback menu)
                                                     menu item)
                                            (change-to-tab *lem-panel* "Main")
                                            (lem:redraw-display)))))
    (change-class menu 'menu :panel multi-column-list-panel)
    (with-apply-in-pane-process-wait-single (*lem-panel*)
      (add-tab-layout *lem-panel* name multi-column-list-panel)
      (change-to-tab *lem-panel* name))))

(defmethod lem-if:update-menu ((implementation capi-impl) menu items)
  (let ((panel (menu-panel menu)))
    (setf (capi:collection-items panel) items)))
