(in-package #:tinymath)

;; --------- Simplification strategies for Tree Operations

(defun simplify! (node)
  "Apply simplification rules to the computation tree rooted at NODE."
  (cond
    ;; If node is nil, return nil
    ((null node) nil)
    
    ;; If node is an atom or symbol, return as is
    ((or (atomp node) (typep node 'sym-node)) node)
    
    ;; If node is an operator, process its children
    ((opp node)
     (simplify-binary node))
    
    (t (error "Unknown node type: ~A" (type-of node)))))

(defun simplify-binary (op-node)
  "Apply simplification rules to a binary operation node."
  (let* ((left  (simplify! (node-left op-node)))   ; Recursively simplify children
         (right (simplify! (node-right op-node)))
         (op    (node-value op-node)))
    ;; Now apply rules to this node based on operator
    (cond
      ;; Identity: x + 0 = x, 0 + x = x
      ((and (eq op '+) (zero? left)) right)
      ((and (eq op '+) (zero? right)) left)
      
      ;; Identity: x * 1 = x, 1 * x = x
      ((and (eq op '*) (one? left)) right)
      ((and (eq op '*) (one? right)) left)
      
      ;; Annihilation: x * 0 = 0, 0 * x = 0
      ((and (eq op '*) (or (zero? left) (zero? right))) 
       (new-atom 0))
      
      ;; Subtraction: x - x = 0
      ((and (eq op '-) (equal-trees left right))
       (new-atom 0))
      
      ;; Subtraction: x - 0 = x
      ((and (eq op '-) (zero? right))
       left)
      
      ;; Division: x / x = 1
      ((and (eq op '/) (equal-trees left right))
       (new-atom 1))
      
      ;; Division: x / 1 = x
      ((and (eq op '/) (one? right))
       left)
      
      ;; Power: x^0 = 1
      ((and (eq op '^) (zero? right))
       (new-atom 1))
      
      ;; Power: x^1 = x
      ((and (eq op '^) (one? right))
       left)
      
      ;; Power: 1^x = 1
      ((and (eq op '^) (one? left))
       (new-atom 1))
      
      ;; Fold constants: if both are numbers, compute
      ((and (typep left 'atom-node) (typep right 'atom-node) 
            (numberp (node-value left)) 
            (numberp (node-value right)))
       (binary-reduce (new-op op left right)))
      
      ;; No rule applies, return reconstructed node
      (t (new-op op left right)))))
