(ql:quickload :quick-patch)

(quick-patch:register "https://github.com/sharplispers/log4cl"
		      "fe3da517147d023029782ced7cd989ba24f1e62d")


(quick-patch:register "https://github.com/scymtym/esrap.git"
		      "d806138342a6b27327649fd5f36e0fe2e0966867")

(quick-patch:checkout-all "build/quick-patch/")
