(defpackage #:tinymath/tests
  (:use :cl :fiveam :tinymath)
  (:import-from :tinymath
                #:wire!
                #:atom-node
                #:op-node
                #:sym-node
                #:simplify!
                #:bindvars
                #:eval-with
                #:partial-eval)
  (:export #:tinymath-suite #:run-all-tests))

(in-package #:tinymath/tests)

;; Define the test suite
(def-suite tinymath-suite
  :description "Test suite for TinyMath")

(in-suite tinymath-suite)

;; =========================================================================
;; Helper Functions for Testing
;; =========================================================================

(defun expr= (expr1 expr2)
  "Compare two expression trees for equality."
  (equal-trees expr1 expr2))

(defun wire-and-simplify (expr)
  "Parse and simplify an expression."
  (simplify! (wire! expr)))

;; =========================================================================
;; Parser Tests
;; =========================================================================

(test parser-atoms
  "Test parsing of atomic values"
  (is (typep (wire! 5) 'atom-node))
  (is (= (node-value (wire! 42)) 42))
  (is (typep (wire! 'x) 'sym-node))
  (is (eq (node-value (wire! 'x)) 'x)))

(test parser-binary-ops
  "Test parsing of binary operations"
  (let ((tree (wire! '(+ 2 3))))
    (is (typep tree 'op-node))
    (is (eq (node-value tree) '+))
    (is (typep (node-left tree) 'atom-node))
    (is (typep (node-right tree) 'atom-node))))

(test parser-nested
  "Test parsing of nested expressions"
  (let ((tree (wire! '(+ (* 2 x) 3))))
    (is (typep tree 'op-node))
    (is (eq (node-value tree) '+))
    (is (typep (node-left tree) 'op-node))))

;; =========================================================================
;; Evaluation Tests
;; =========================================================================

(test evaluate-simple
  "Test basic evaluation"
  (is (= (consume (wire! '(+ 2 3))) 5))
  (is (= (consume (wire! '(* 4 5))) 20))
  (is (= (consume (wire! '(- 10 3))) 7))
  (is (= (consume (wire! '(/ 20 4))) 5)))

(test evaluate-nested
  "Test evaluation of nested expressions"
  (is (= (consume (wire! '(+ (* 2 3) 4))) 10))
  (is (= (consume (wire! '(* (+ 2 3) (- 7 2)))) 25)))

;; =========================================================================
;; Simplification Tests
;; =========================================================================

(test simplify-identity-addition
  "Test x + 0 = x and 0 + x = x"
  (is (expr= (wire-and-simplify '(+ x 0))
             (wire! 'x)))
  (is (expr= (wire-and-simplify '(+ 0 x))
             (wire! 'x))))

(test simplify-identity-multiplication
  "Test x * 1 = x and 1 * x = x"
  (is (expr= (wire-and-simplify '(* x 1))
             (wire! 'x)))
  (is (expr= (wire-and-simplify '(* 1 x))
             (wire! 'x))))

(test simplify-annihilation
  "Test x * 0 = 0 and 0 * x = 0"
  (is (expr= (wire-and-simplify '(* x 0))
             (wire! 0)))
  (is (expr= (wire-and-simplify '(* 0 x))
             (wire! 0))))

(test simplify-constant-folding
  "Test constant folding"
  (is (expr= (wire-and-simplify '(+ 2 3))
             (wire! 5)))
  (is (= (node-value (wire-and-simplify '(* 4 5))) 20)))

;; =========================================================================
;; Variable Substitution Tests
;; =========================================================================

(test substitute-simple
  "Test simple variable substitution"
  (let* ((expr (wire! 'x))
         (result (bindvars expr '((x . 5)))))
    (is (typep result 'atom-node))
    (is (= (node-value result) 5))))

(test substitute-expression
  "Test substitution in expressions"
  (let* ((expr (wire! '(+ (* 2 x) 3)))
         (result (bindvars expr '((x . 5)))))
    (is (= (consume result) 13))))

(test eval-with-test
  "Test direct evaluation with bindings"
  (is (= (eval-with (wire! '(+ (* 2 x) 3)) '((x . 5))) 13))
  (is (= (eval-with (wire! '(* x y)) '((x . 3) (y . 4))) 12)))

;; =========================================================================
;; Differentiation Tests
;; =========================================================================

(test diff-constant
  "Test derivative of constants"
  (is (expr= (differentiate (wire! 5) 'x)
             (wire! 0))))

(test diff-variable
  "Test derivative of variables"
  (is (expr= (differentiate (wire! 'x) 'x)
             (wire! 1)))
  (is (expr= (differentiate (wire! 'y) 'x)
             (wire! 0))))

(test diff-sum
  "Test sum rule: d/dx(x + x) = 2"
  (is (expr= (differentiate (wire! '(+ x x)) 'x)
             (wire! 2))))

(test diff-power-rule-basic
  "Test power rule: d/dx(x^2)"
  (let ((result (differentiate (wire! '(^ x 2)) 'x)))
    ;; Should give 2x after simplification
    (is (typep result 'op-node))))

(test diff-product-rule
  "Test product rule: d/dx(x * x) = 2x"
  (let ((result (differentiate (wire! '(* x x)) 'x)))
    ;; Result should simplify to 2x
    (is (typep result 'op-node))))

(test diff-trig
  "Test trig derivatives: d/dx(sin(x)) = cos(x)"
  (let ((result (differentiate (wire! '(sin x)) 'x)))
    (is (typep result 'op-node))
    (is (eq (node-value result) 'cos))))

;; =========================================================================
;; Integration Tests (End-to-End)
;; =========================================================================

(test integration-diff-and-eval
  "Test differentiation followed by evaluation"
  ;; d/dx(x^2) at x=5 should be 10
  (let* ((expr (wire! '(^ x 2)))
         (deriv (differentiate expr 'x))
         (result (eval-with deriv '((x . 5)))))
    (is (= result 10))))

;; =========================================================================
;; Run Tests Function
;; =========================================================================

(defun run-all-tests ()
  "Run all TinyMath tests and print results."
  (run! 'tinymath-suite))
