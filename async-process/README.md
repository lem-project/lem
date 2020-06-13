# async-process

```
CL-USER> (ql:quickload :async-process)
To load "async-process":
  Load 1 ASDF system:
    async-process
; Loading "async-process"
..................................................
[package async-process].
(:ASYNC-PROCESS)
CL-USER> (in-package async-process)
#<PACKAGE "ASYNC-PROCESS">
ASYNC-PROCESS> (create-process "python")
#.(SB-SYS:INT-SAP #X7FFFEC002830)
ASYNC-PROCESS> (defparameter p *)
#.(SB-SYS:INT-SAP #X7FFFEC002830)
ASYNC-PROCESS> (process-receive-output p)
"Python 2.7.13 (default, Nov 24 2017, 17:33:09) 
[GCC 6.3.0 20170516] on linux2
Type \"help\", \"copyright\", \"credits\" or \"license\" for more information.
>>> "
ASYNC-PROCESS> (process-send-input p "1+1
")
; No value
ASYNC-PROCESS> (process-receive-output p)
"1+1
2
>>> "
```

## LICENSE
MIT
