open! Base

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

(* Examples with [pexp_hole] *)

type s = float Map.M(Char).t Map.M(String).t Map.M(Int).t Map.M(Bool).t

let map : s = Map.empty (module Bool)

let _flip =
  map
  |> Map.find_exn _ true
  |> Map.find_exn _ 2
  |> Map.find_exn _ "3"
  |> Map.find_exn _ '4'
;;

let f x y z = x + y + z
let _fun = 2 |> f 1 _ 3
let f ~x = x
let _op = 2 |> _ - 3
