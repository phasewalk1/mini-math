(asdf:clear-configuration)
(asdf:clear-source-registry)

(ql:quickload :tinymath)
(ql:quickload :tinymath/tests)

(tinymath/tests:run-all-tests)
