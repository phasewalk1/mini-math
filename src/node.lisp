(in-package #:tinymath)

;; We use binary trees to represent algebraic expressions in such a way
;; that closely models the underlying data structure Lisp uses for lists.
;; 
;; In our model, the nodes in the tree can be of the following types:
;;   - atom-node:  a numeric value
;;   - sym-node:   a symbol, can be variable or point to a function (e.g., 'x' or 'sin')
;;   - op-node:    builtin operator (e.g., '+', '*', etc.)

;; --------- Binary Operators ------------------------------------------------------
;;
;; The following is an example of a computation tree for the expression 
;;   2x + 3   
;;      or   
;; (+ (* 2 x) 3)
;;
;;       +                +              
;;      / \              / \
;;     *   3    ---->   2x  3    ---->    2x + 3
;;    / \
;;   2   x
;;
;; We call operations such as addition, multiplication, division, etc., binary operations
;; because they act on two inputs (left and right). 
;;     
;; --------- Unary Operators -------------------------------------------------------
;;
;; To include unary operations like trigonometric functions or logarithms within our binary tree structure,
;; we adapt the representation to accommodate these operations. Unary operations inherently require only one operand,
;; and we represent them in our binary tree by assigning the operand to one branch (typically the left for consistency),
;; and using the other branch (right) to hold a placeholder or `nil`, indicating the absence of a second operand.
;;
;; For example, the expression:
;;     sin(x) + 3
;;         or 
;;     (+ (sin x) 3)
;;
;; Can be represented as:
;;
;;       +                
;;      / \              
;;    sin  3     
;;    /
;;   x   
;;
;; Here, the `sin` node represents the unary operation, with `x` as its operand (left child), and an implicit `nil` on 
;; the right. This approach allows us to uniformly handle both unary and binary operations 
;; within the same binary tree structure, facilitating parsing, manipulation, and evaluation of algebraic expressions.

;; --------- Node Definition (CLOS Class Hierarchy)
;;   Base class: node
;;   Specialized classes: atom-node, sym-node, op-node

(defclass node ()
  ((left  :initarg :left  :accessor node-left  :initform nil)
   (right :initarg :right :accessor node-right :initform nil)))

(defclass atom-node (node)
  ((value :initarg :value :accessor node-value)))

(defclass sym-node (node)
  ((value :initarg :value :accessor node-value)))

(defclass op-node (node)
  ((value :initarg :value :accessor node-value)))

(defun new-atom (value)
  (unless (numberp value)
    (error "Invalid atom type: ~A" value))
  (make-instance 'atom-node :value value :left nil :right nil))

(defun new-op (op left right)
  "Create an operator node."
  (make-instance 'op-node :value op :left left :right right))

(defun new-sym (symbol)
  "Create a symbol node (variable)."
  (make-instance 'sym-node :value symbol :left nil :right nil))

;; Predicates
(defun atomp (node)
  (typep node 'atom-node))

(defun opp (node)
  (typep node 'op-node))
