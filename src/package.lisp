(defpackage #:tinymath
  (:use :cl)
  (:export
   ;; Node constructors
   #:new-atom
   #:new-sym
   #:new-op
   
   ;; Node accessors
   #:node-value
   #:node-left
   #:node-right
   
   ;; Core functions
   #:wire!
   #:consume
   #:bindvars
   #:eval-with
   #:partial-eval

   ;; Simplification strategies
   #:simplify!
   
   ;; Utilities
   #:show-tree
   #:equal-trees
   #:atomp
   #:opp
   #:zero?
   #:one?
   
   ;; Derivatives
   #:differentiate))
