(asdf:defsystem #:tinymath
  :description "A symbolic mathematics system"
  :author "phasewalk <ethgallucci@berkeley.edu>"
  :license  "MIT"
  :version "0.1.0"
  :serial t
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "node")
                 (:file "utils")
                 (:file "wire")
                 (:file "eval")
                 (:file "simplify")
                 (:file "derivative"))))
  :in-order-to ((test-op (test-op #:tinymath/tests))))

;; Test system
(asdf:defsystem #:tinymath/tests
  :depends-on (#:tinymath #:fiveam)
  :serial t
  :components ((:module "tests"
                :components
                ((:file "engine"))))
  :perform (test-op (o c) 
             (symbol-call :fiveam '#:run! 
                          (find-symbol* '#:tinymath-suite 
                                        :tinymath/tests))))
