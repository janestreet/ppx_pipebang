let f x = x
let g x = x
let h x = x
let _ = 1 |> f |> (g |> h)

type 'a t =
  | X
  | F of 'a

let _constr_on_left f = X |> f
let _constr_on_right x = x |> F

let _constr_on_left_with_attr f =
  ((X [@ppwarning "warning should be suppressed"]) |> f) [@warning "-preprocessor"]
;;

let _constr_on_right_with_attr x =
  ((x [@ppwarning "warning should be suppressed"]) |> F) [@warning "-preprocessor"]
;;
