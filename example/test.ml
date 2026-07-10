open! Base

let f x = x
let g x = x
let h x = x
let _foo = 1 |> f |> (g |> h)

type 'a t =
  | X
  | F of 'a
  | Tuple of int * int
  | Record of
      { a : int
      ; b : int
      }

let _constr_on_left f = X |> f
let _constr_on_right x = x |> F
let _constr_tuple x = x |> (2, _) |> Tuple
let _constr_record x = x |> { a = 2; b = _ } |> Record

let _constr_on_left_with_attr f =
  ((X [@ppwarning "warning should be suppressed"]) |> f) [@warning "-preprocessor"]
;;

let _constr_on_right_with_attr x =
  ((x [@ppwarning "warning should be suppressed"]) |> F) [@warning "-preprocessor"]
;;

(* Examples with [pexp_hole] *)

type s = float Map.M(Char).t Map.M(String).t Map.M(Int).t Map.M(Bool).t

let map : s = Map.empty (module Bool)

let _flip _ =
  map
  |> Map.find_exn _ true
  |> Map.find_exn _ (2 |> Fn.id _)
  |> Map.find_exn _ "3"
  |> Map.find_exn _ '4'
;;

let f x y z = x + y + z
let _fun = 2 |> f 1 _ 3
let _op = 2 |> _ - 3
let _polymorphic_variant = 2 |> (_, 1) |> `Foo _
let _constr = 2 |> F _
let _labelled_arg = 1 |> Map.set (Map.empty (module Int)) ~key:_ ~data:"x"

type record =
  { x : int
  ; y : string
  }

type record2 = { foo : record# }

let _unboxed_tuple = 3 |> #(_, 15)
let _record_field = 2 |> { x = _; y = "s" } |> _.x
let _unboxed_record_field = 2 |> #{ x = _; y = "s" } |> _.#x
let _field_setter ref = ref |> (_.contents <- 5)
let _field_setter_value ref = 5 |> (ref.contents <- _)
let _array = 14 |> [| 1; _; 15 |]
let _assert = true |> assert _
let _constraint n = n |> (_ : _ @ contended)
let _coerce n = n |> (_ :> int)
let _index = (.foo) |> (.idx_imm(_).#x)
let _borrow x f = x |> borrow_ _ |> f _
let _send obj = obj |> (_)#foo [@@ocamlformat "disable"]

let _override =
  object
    val foo = 14
    method m = 15 |> {<foo = _>}
  end
;;

let _merge_attributes f =
  ((X [@ppwarning "warning should be suppressed"]) |> f _) [@warning "-preprocessor"]
;;
