(in-package #:tinymath)

;; ------- Wires S-expressions to computation trees
;; --> Outputs the root of the computation tree
(defun wire! (expr) 
  "Convert an S-expression into a computation tree."
  (cond
    ;; Base case: number
    ((numberp expr) 
     (new-atom expr))
    
    ;; Base case: symbol (variable)
    ((symbolp expr) 
     (new-sym expr))
    
    ;; Recursive case: list (operation)
    ((listp expr)
     (let ((op (first expr)))
       (cond
         ;; Binary operators
         ((member op '(+ - * / ^))
          (new-op op 
                  (wire! (second expr))   ; recursively wire left
                  (wire! (third expr))))  ; recursively wire right
         
         ;; Unary operators (sin, cos, etc.)
         ((member op '(sin cos tan ln exp sqrt))
          (new-op op 
                  (wire! (second expr))   ; wire the operand
                  nil))                  ; no right child
         
         (t (error "Unknown operator: ~A" op)))))
    
    (t (error "Cannot parse expression: ~A" expr)))) 

