(in-package #:tinymath)

;; Helper predicates for simplification
(defun zero? (node)
  "Check if node represents the constant 0."
  (and (typep node 'atom-node) (= (node-value node) 0)))

(defun one? (node)
  "Check if node represents the constant 1."
  (and (typep node 'atom-node) (= (node-value node) 1)))

(defun equal-trees (n1 n2)
  "Check if two trees are structurally equal."
  (cond
    ((and (null n1) (null n2)) t)
    ((or (null n1) (null n2)) nil)
    ((not (eq (type-of n1) (type-of n2))) nil)
    ((not (equal (node-value n1) (node-value n2))) nil)
    (t (and (equal-trees (node-left n1) (node-left n2))
            (equal-trees (node-right n1) (node-right n2))))))

;; --------- Tree Visualization
(defun show-tree (tree &optional (indent 0))
  "Pretty-print the tree."
  (when tree
    (show-tree (node-right tree) (+ indent 4))
    (format t "~v@T~A: ~A~%" indent (type-of tree) (node-value tree))
    (show-tree (node-left tree) (+ indent 4))))
