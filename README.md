ppx_pipebang
============

A ppx rewriter that inlines the reverse application operator `|>`.

`ppx_pipebang` rewrites `x |> f` as `f x`, regardless of whether `|>` has been redefined.

This inlining is mostly done for historical reasons but it also allows `f` to have
optional arguments (like `Option.value_exn`).

`ppx_pipebang` special cases `pexp_hole`s such that your value does not need to be piped
as the last argument. For example, `x |> f _ y` is rewritten as `f x y`. This also works
for labelled arguments, rewriting `x |> f ~lbl:_` as `f ~lbl:x`, and for a variety of
other expression shapes. Given:

```ocaml
type t =
  | F of int
  | Tuple of int * int
  | Record of { a : int; b : int }

type record = { c : int }
```

some common examples:

```ocaml
let _constr x = x |> F _
let _tuple x = x |> (2, _) |> Tuple
let _record x = x |> { a = 2; b = _ } |> Record
let _field_get x = x |> _.c
let _coerce n = n |> (_ :> int)
```

See `example/test.ml` for the full list of supported shapes. At most one hole may appear
in any of these forms; using more than one is a compile-time error.

## Caveat: depth of replacement

The hole replacement is shallow: `ppx_pipebang` only looks one level into the right-hand
side. In particular, for constructors and polymorphic variants, the hole has to be the
constructor's argument itself — a hole nested inside a tuple or record argument is not
found. So `x |> Tuple (2, _)` does not work; you must pipe through the tuple first, as in
`x |> (2, _) |> Tuple` (and similarly for records).

### Motivation

There may one day be a (not yet fully-designed) OxCaml feature that replaces `foo _ bar`
with `fun x -> foo x bar` without involvement from PPXs. If we pledged to resolve holes at
arbitrary depths, the behavior would become ambiguous: does `foo (bar _ baz)` mean
`fun x -> foo (bar x baz)` or `foo (fun x -> bar x baz)`? To avoid that confusion, and to
stay forward-compatible with the planned feature, we limit the depth of resolution to one
level.

Even where the compiler will eventually be able to disambiguate, the PPX cannot, because
it runs before type-checking. For example, consider:

```ocaml
type t =
  | Inline_tuple of int * int
  | Single_tuple_argument of (int * int)
  | Tuple_fun_argument of (int -> int * int)
```

At the call site, `Inline_tuple (_, 2)`, `Single_tuple_argument (_, 2)`, and
`Tuple_fun_argument (_, 2)` are syntactically identical (all parse as a constructor
applied to a tuple), so a syntactic PPX cannot tell them apart. The compiler, with type
information, will be able to correctly desugar `Inline_tuple (_, 2)` to
`fun x -> Inline_tuple (x, 2)`. On the other hand, it's reasonable that the correct
desugaring of `Tuple_fun_argument (_, 2)` is `Tuple_fun_argument (fun x -> (x, 2))`.
But the PPX would have to guess between the two. We side-step that by requiring the user
to break the expression apart, e.g. `x |> (_, 2) |> Single_tuple_argument`,
which is unambiguous.
