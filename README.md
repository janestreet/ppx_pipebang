ppx_pipebang
============

A ppx rewriter that inlines the reverse application operator `|>`.

`ppx_pipebang` rewrites `x |> f` as `f x`, regardless of whether `|>` has been redefined.

This inlining is mostly done for historical reasons but it also allows `f` to have
optional arguments (like `Option.value_exn`).

`ppx_pipebang` special cases `pexp_hole`s such that your value does not need to be piped
as the last argument. For example, `x |> f _ y` is rewritten as `f x y`. This also works
for labelled arguments, rewriting `x |> f ~lbl:_` as `f ~lbl:x`.
