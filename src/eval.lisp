(in-package #:tinymath)

;; Reduce the binary expression (i.e., "(+ 5 1)") to a single atomic value
;; (i.e., "6").
(defun binary-reduce (operator-node)
  "Reduce a binary operation node to a single atomic value."
  (let* ((left  (node-left operator-node))
         (right (node-right operator-node))
         (op    (node-value operator-node)))
    (when (typep left 'op-node)
      (setf left (binary-reduce left)))
    (when (typep right 'op-node)
      (setf right (binary-reduce right)))
    (unless (and (typep left 'atom-node)
                 (typep right 'atom-node))
      (error "Both left and right nodes must be atoms for binary reduction."))
    (let ((x (node-value left))
          (y (node-value right)))
      (cond
        ((eq op '+) (new-atom (+ x y)))
        ((eq op '-) (new-atom (- x y)))
        ((eq op '*) (new-atom (* x y)))
        ((eq op '/) (new-atom (/ x y)))
        ((eq op '^) (new-atom (expt x y)))
        (t (error "Unsupported operator: ~A" op))))))

;; --------- Consume: reduces a computation tree from root
(defun consume (root)
  (cond
    ((atomp root)
     (node-value root))
    ((opp root)
     (node-value (binary-reduce root)))))

;; --------- Variable Substitution and Evaluation
;;
;; This system provides functionality to substitute variables in an expression tree
;; with concrete values and evaluate the result.
;;
;; For example, given the expression tree for "2x + 3" and the binding (x . 5),
;; we can substitute to get "2(5) + 3" and evaluate to get 13.

(defun bindvars (node bindings)
  "Substitute variables in NODE according to BINDINGS (an association list).
   Returns a new tree with variables replaced by their values.
   
   Example: (bindvars (wire! '(+ (* 2 x) 3)) '((x . 5)))
            => tree representing (+ (* 2 5) 3)"
  (cond
    ((typep node 'atom-node)
     node)

    ((typep node 'sym-node)
     (let ((binding (assoc (node-value node) bindings)))
       (if binding
         (new-atom (cdr binding))
         node)))

    ((typep node 'op-node)
     (new-op (node-value node)
             (bindvars (node-left node) bindings)
             (when (node-right node)
               (bindvars (node-right node) bindings))))
    (t (error "Unknown node type: ~A" (type-of node)))))

(defun eval-with (node bindings)
  "Evaluate an expression tree with variable bindings.
   This combines substitution and evaluation into one step.
   
   Example: (evaluate-with-bindings (wire! '(+ (* 2 x) 3)) '((x . 5)))
            => 13"
  (let ((substituted (bindvars node bindings)))
    (consume substituted)))

(defun partial-eval (node bindings)
  "Partially evaluate and simplify an expression with some variable bindings.
   Variables not in BINDINGS remain symbolic.
   
   Example: (partial-eval (wire! '(+ (* 2 x) (* 3 y))) '((x . 5)))
            => tree for (+ 10 (* 3 y))"
  (let ((substituted (bindvars node bindings)))
    (apply-rules substituted)))
