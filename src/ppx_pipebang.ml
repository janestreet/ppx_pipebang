open! Ppxlib

let is_unlabelled_pexp_hole (lbl, e) ~loc =
  match
    (lbl : arg_label), Ppxlib_jane.Shim.Expression_desc.of_parsetree e.pexp_desc ~loc
  with
  | Nolabel, Pexp_hole -> true
  | _ -> false
;;

let exactly_one_unlabelled_pexp_hole args ~loc =
  List.filter (is_unlabelled_pexp_hole ~loc) args
  |> function
  | [ _ ] -> true
  | _ -> false
;;

let replace_unlabelled_pexp_hole args x ~loc =
  List.map (fun arg -> if is_unlabelled_pexp_hole arg ~loc then Nolabel, x else arg) args
;;

let expand (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_apply (_, [ (Nolabel, x); (Nolabel, y) ]) ->
    Some
      (match y with
       | { pexp_desc = Pexp_construct (id, None); _ } ->
         { e with pexp_desc = Pexp_construct (id, Some x) }
       | { pexp_desc = Pexp_apply (f, args); pexp_attributes = []; pexp_loc = loc; _ }
         when match f.pexp_desc with
              (* Do not inline |> as this would create applications with too many
                 arguments *)
              | Pexp_ident { txt = Lident "|>"; _ } -> false
              | _ -> true ->
         (match exactly_one_unlabelled_pexp_hole args ~loc with
          | false -> { e with pexp_desc = Pexp_apply (f, args @ [ Nolabel, x ]) }
          | true ->
            let args = replace_unlabelled_pexp_hole args x ~loc in
            { e with pexp_desc = Pexp_apply (f, args) })
       | _ -> { e with pexp_desc = Pexp_apply (y, [ Nolabel, x ]) })
  | Pexp_ident { txt = Lident s; _ }
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident s; _ }; _ }, _) ->
    Location.raise_errorf ~loc:e.pexp_loc "%s must be applied to two arguments" s
  | _ -> None
;;

let () =
  Driver.register_transformation
    "pipebang"
    ~rules:[ Context_free.Rule.special_function "|>" expand ]
;;
