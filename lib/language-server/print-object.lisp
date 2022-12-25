(in-package :lem-language-server)

(defmethod print-object ((object protocol:position) stream)
  (print-unreadable-object (object stream :type t)
    (format stream
            "line:~D character:~D"
            (protocol:position-line object)
            (protocol:position-character object))))

(defmethod print-object ((object protocol:range) stream)
  (print-unreadable-object (object stream :type t)
    (format stream
            "start: ~S end: ~S"
            (protocol:range-start object)
            (protocol:range-end object))))
