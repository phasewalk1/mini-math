(in-package #:tinymath)

;; --------- Symbolic Differentiation
;;
;; This module implements automatic symbolic differentiation using the standard
;; calculus rules. The differentiation process works recursively on the expression
;; tree structure, applying the appropriate rule at each node.
;;
;; Key differentiation rules implemented:
;;   - Constant rule:   d/dx(c) = 0
;;   - Variable rule:   d/dx(x) = 1, d/dx(y) = 0
;;   - Sum rule:        d/dx(f + g) = f' + g'
;;   - Difference rule: d/dx(f - g) = f' - g'
;;   - Product rule:    d/dx(f * g) = f' * g + f * g'
;;   - Quotient rule:   d/dx(f / g) = (f' * g - f * g') / g^2
;;   - Power rule:      d/dx(f^n) = n * f^(n-1) * f' (chain rule included)
;;   - Chain rule:      d/dx(f(g(x))) = f'(g(x)) * g'(x)
;;
;; The result is automatically simplified using the simplification engine.

(defun differentiate (node var)
  "Differentiate the expression tree NODE with respect to variable VAR."
  (simplify!
    (or (constant-rule node var)
        (variable-rule node var)
        (sum-rule node var)
        (difference-rule node var)
        (product-rule node var)
        (quotient-rule node var)
        (power-rule node var)
        (trig-rule node var)
        (exp-log-rule node var)
        (error "No differentiation rule applies to node: ~A" node))))

(defun constant-rule (node var)
  "Apply the constant rule: d/dx(c) = 0"
  (when (and (typep node 'atom-node)
             (numberp (node-value node)))
    (new-atom 0)))

(defun variable-rule (node var)
  "Apply the variable rule: d/dx(x) = 1, d/dx(y) = 0"
  (when (typep node 'sym-node)
    (if (eq (node-value node) var)
        (new-atom 1)
        (new-atom 0))))

(defun sum-rule (node var)
  "Apply the sum rule: d/dx(f + g) = f' + g'"
  (when (and (opp node) (eq (node-value node) '+))
    (let ((f-prime (differentiate (node-left node) var))
          (g-prime (differentiate (node-right node) var)))
      (new-op '+ f-prime g-prime))))

(defun difference-rule (node var)
  "Apply the difference rule: d/dx(f - g) = f' - g'"
  (when (and (opp node) (eq (node-value node) '-))
    (let ((f-prime (differentiate (node-left node) var))
          (g-prime (differentiate (node-right node) var)))
      (new-op '- f-prime g-prime))))

(defun product-rule (node var)
  "Apply the product rule: d/dx(f * g) = f' * g + f * g'"
  (when (and (opp node) (eq (node-value node) '*))
    (let* ((f (node-left node))
           (g (node-right node))
           (f-prime (differentiate f var))
           (g-prime (differentiate g var)))
      (new-op '+
              (new-op '* f-prime g)
              (new-op '* f g-prime)))))

(defun quotient-rule (node var)
  "Apply the quotient rule: d/dx(f / g) = (f' * g - f * g') / g^2"
  (when (and (opp node) (eq (node-value node) '/))
    (let* ((f (node-left node))
           (g (node-right node))
           (f-prime (differentiate f var))
           (g-prime (differentiate g var)))
      (new-op '/
              (new-op '-
                      (new-op '* f-prime g)
                      (new-op '* f g-prime))
              (new-op '^ g (new-atom 2))))))

(defun power-rule (node var)
  "Apply the power rule: d/dx(f^n) = n * f^(n-1) * f'"
  (when (and (opp node) (eq (node-value node) '^))
    (let* ((f (node-left node))
           (n (node-right node)))
      (cond
        ;; Case 1: n is a constant - standard power rule
        ((typep n 'atom-node)
         (let ((n-val (node-value n)))
           (new-op '*
                   (new-op '*
                           n  ; The exponent n
                           (new-op '^ f (new-atom (- n-val 1))))  ; f^(n-1)
                   (differentiate f var))))  ; f'
        
        ;; Case 2: General case - logarithmic differentiation
        ;; d/dx(f^g) = f^g * (g' * ln(f) + g * f'/f)
        (t
         (new-op '*
                 node  ; f^g
                 (new-op '+
                         (new-op '*
                                 (differentiate n var)  ; g'
                                 (new-op 'ln f nil))    ; ln(f)
                         (new-op '*
                                 n  ; g
                                 (new-op '/
                                         (differentiate f var)  ; f'
                                         f)))))))))  ; f

(defun trig-rule (node var)
  "Apply chain rule for trigonometric functions."
  (when (opp node)
    (let ((op (node-value node))
          (f (node-left node)))
      (cond
        ;; d/dx(sin(f)) = cos(f) * f'
        ((eq op 'sin)
         (new-op '*
                 (new-op 'cos f nil)
                 (differentiate f var)))
        
        ;; d/dx(cos(f)) = -sin(f) * f'
        ((eq op 'cos)
         (new-op '*
                 (new-op '*
                         (new-atom -1)
                         (new-op 'sin f nil))
                 (differentiate f var)))
        
        ;; d/dx(tan(f)) = sec^2(f) * f' = (1/cos^2(f)) * f'
        ((eq op 'tan)
         (new-op '*
                 (new-op '/
                         (new-atom 1)
                         (new-op '^
                                 (new-op 'cos f nil)
                                 (new-atom 2)))
                 (differentiate f var)))))))

(defun exp-log-rule (node var)
  "Apply chain rule for exponential and logarithmic functions."
  (when (opp node)
    (let ((op (node-value node))
          (f (node-left node)))
      (cond
        ;; d/dx(ln(f)) = f' / f
        ((eq op 'ln)
         (new-op '/
                 (differentiate f var)
                 f))
        
        ;; d/dx(e^f) = e^f * f'
        ((eq op 'exp)
         (new-op '*
                 node  ; e^f
                 (differentiate f var)))
        
        ;; d/dx(sqrt(f)) = f' / (2 * sqrt(f))
        ((eq op 'sqrt)
         (new-op '/
                 (differentiate f var)
                 (new-op '*
                         (new-atom 2)
                         node)))))))  ; sqrt(f)

;; --------- Higher-Order Derivatives

(defun nth-derivative (node var n)
  "Compute the n-th derivative of NODE with respect to VAR."
  (if (<= n 0)
      node
      (nth-derivative (differentiate node var) var (1- n))))

(defun gradient (node vars)
  "Compute the gradient of NODE with respect to all variables in VARS.
   Returns a list of derivative trees.
   
   Example: (gradient (wire '(+ (* x x) (* y y))) '(x y))
            => (tree for 2x, tree for 2y)"
  (mapcar (lambda (var) (differentiate node var)) vars))
