# `nix-eval`

### An evaluator for the nix language in Haskell

## Overview

This is an implementation of a nix language evaluator in Haskell.
Currently, it only does the pure part of evaluation; that is, the
generation of derivations, but not the realization of those
derivations.

## Why Write This?

There are a few reasons for this.

* It might provide the groundwork for a full nix implementation in
  Haskell.
* It should be considerably simpler and easier to read than the
  official C++ implementation. I'm endeavoring to use existing
  libraries as much as possible for things like internal data
  structures and built-in functions, and also attempting to document
  the code clearly.
* Writing evaluators and language implementations is fun, especially
  in Haskell. Also, since the evaluator is pure, there shouldn't be
  any need for IO (except at initialization for things like
  `builtins.currentSystem`).
* I've never written a lazy evaluator before. I'm sort of figuring
  things out as I go along, so this is a fun learning experience for
  me. In addition, figuring out how best to implement built-in
  functions and (perhaps) identifying techniques for optimization,
  sounds fun.

## Grammar

`nix-eval` does not attempt to evaluate the full nix grammar (as, for
instance, is expressed in the
[`hnix` library](https://github.com/jwiegley/hnix)) directly. Instead,
it defines a simpler grammar for nix, which is easier to evaluate:

```haskell
-- Found in src/Nix/Eval/Expressions.hs
data Expression
  = EConstant Constant
  | EVar Text
  | EListLiteral [Expression]
  | ENonRecursiveAttrs (HashMap Text Expression)
  | ERecursiveAttrs (HashMap Text Expression)
  | EAttrReference Expression Text
  | EBinaryOp Expression BinaryOp Expression
  | EUnaryOp UnaryOp Expression
  | ELambda Text Expression
  | EApply Expression Expression
  | EWith Expression Expression
```

For this reason, the library does not provide a parser for
human-written nix language: the parsing should be done by `hnix` (or
an equivalent) and then translated into the `nix-eval` grammar. For
example, `inherit (x) y z;` forms can be converted into `y = x.y; z =
x.z;`, etc. Even the `let` expression in nix can be viewed as
syntactic sugar for a `with` expression.
