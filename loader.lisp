(require :asdf)
(require :lem)
(apply 'lem:lem (cdr sb-ext:*posix-argv*))
