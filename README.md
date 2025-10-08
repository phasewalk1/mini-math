# nform

*nform* is a lightweight symbolic computation engine written entirely in **Common Lisp**. It performs **symbolic differentiation**, simplification, variable binding/substitution, **partial evaluation**, and **expression rewriting** over algebraic expression trees.

## Conceptual Foundation

nform is built on the principle that **computation = term rewriting**. Each simplification or differentiation rule is a **directed rewrite**:
```lisp
(* x 0) -> 0
(+ 0 x) -> x
(^ x 1) -> x
```

By abstracting these as declarative `defrule` definitions, nform will evolve into a **term rewriting system (TRS)** - the same conceptual model that underlies theorem provers and symbolic AI systems like **Lean**, **Coq**, and **Mathematica**.

## Overview

nform represents computation as generic term rewriting over algebraic expression trees, which we call *computation trees*. Under the hood, these are simple binary trees. Nodes can take on one of the following types:
- `atom`: a numeric value
- `sym`: a symbol, can be a variable such as `x`, or point to a unary function (e.g., `sin`)
- `op`: binary operators (e.g., `+`, `-`, `*`, `/`, `^`, etc.)

The following is an example of a computation tree for the expression: `2x + 3`, or `(+ (* 2 x) 3)`:

```lisp

       +                +              
      / \              / \
     *   3    ---->   2x  3    ---->    2x + 3
    / \
   2   x
```

This structure allows the system to **manipulate algebraic expressions symbolically**, rather than numerically, enabling:
- **Differentiation** by calculus rules,
- **Simplification** by pattern-based identities,
- **Evaluation** via variable substitution,
- and soon — **general term rewriting** for equational reasoning.

### Why “nform”?

In symbolic algebra and logic, a **normal form** is a canonical representation of an expression —  
a state where no further simplifications or rewrites can be applied.  

Every simplification, differentiation, or substitution in *nform* is a transformation toward this **normal form**.  
The name reflects the system’s guiding idea:

> **Computation is normalization.**  
> Reducing expressions to their essential, irreducible structure.

That principle connects *nform* to the foundations of:
- **λ-calculus**, where β-reduction leads to normal form,  
- **term rewriting systems**, which formalize algebraic simplification, and  
- **theorem provers** like *Lean* and *Coq*, which verify proofs by normalizing terms.

*nform* is thus not just a symbolic differentiator — it’s a step toward a general rewriting engine for symbolic reasoning and equational logic.

## Roadmap
| Milestone                | Description                                                       |
| ------------------------ | ----------------------------------------------------------------- |
| [X] Core tree system       | Node hierarchy and constructors                                   |
| [X] Differentiation engine | Rule-based calculus                                               |
| [X] Simplifier             | Algebraic and constant folding                                    |
| [X] Variable substitution  | Binding + partial evaluation                                      |
| [ ] Rewrite system        | General pattern-matching rule engine                              |
| [ ] Equational reasoning  | Canonical form + expression equality                              |
| [ ] Web REPL              | Interactive demo via Lisp web server                              |
| [ ] Visualization         | Graphviz AST visualization                                        |
| [ ] Autograd / crypto     | Symbolic automatic differentiation or modular algebra experiments |
