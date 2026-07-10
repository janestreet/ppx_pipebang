open! Ppxlib

let is_pipe_application expression =
  match expression.pexp_desc with
  | Pexp_apply ([%expr ( |> )], _) -> true
  | _ -> false
;;

let replace expression ~replacement =
  let loc = { expression.pexp_loc with loc_ghost = true } in
  { replacement with
    pexp_loc = loc
  ; pexp_loc_stack = replacement.pexp_loc :: expression.pexp_loc_stack
  ; pexp_attributes = replacement.pexp_attributes @ expression.pexp_attributes
  }
;;

let is_hole expression =
  match
    Ppxlib_jane.Shim.Expression_desc.of_parsetree
      expression.pexp_desc
      ~loc:expression.pexp_loc
  with
  | Pexp_hole -> true
  | _ -> false
;;

let error_extensionf ~loc =
  Printf.ksprintf (fun error ->
    let loc = { loc with loc_ghost = true } in
    Location.error_extensionf ~loc "ppx_pipebang: %s" error
    |> Ast_builder.Default.pexp_extension ~loc)
;;

let replace_hole_maybe expression ~replacement =
  let maybe_replace ?(used = `Not_used) expression =
    if is_hole expression
    then (
      let expression =
        match used with
        | `Used ->
          error_extensionf
            ~loc:expression.pexp_loc
            "not expecting an expression hole here, expression hole already specified \
             elsewhere."
        | `Not_used -> replace expression ~replacement
      in
      `Used, expression)
    else used, expression
  in
  let some_if_used ~used (value : Ppxlib_jane.Shim.Expression_desc.t) =
    match used with
    | `Used -> Some (Ok value)
    | `Not_used -> None
  in
  let maybe_replace_list_with_labels ?(used = `Not_used) list =
    ListLabels.fold_left_map list ~init:used ~f:(fun used (label, expr) ->
      let used, expr = maybe_replace ~used expr in
      used, (label, expr))
  in
  let result =
    match
      Ppxlib_jane.Shim.Expression_desc.of_parsetree
        expression.pexp_desc
        ~loc:expression.pexp_loc
    with
    | Pexp_hole (* [ x |> _ ] *) ->
      Some (Error "not expecting an expression hole here, `|> _` not supported.")
    | Pexp_construct (constructor, Some arg) (* [ x |> Foo _ ] *) ->
      let used, arg = maybe_replace arg in
      some_if_used ~used (Pexp_construct (constructor, Some arg))
    | Pexp_variant (constructor, Some arg) (* [ x |> `Foo _ ] *) ->
      let used, arg = maybe_replace arg in
      some_if_used ~used (Pexp_variant (constructor, Some arg))
    | Pexp_apply (func, args)
    (* {[
         x |> foo _ bar;;
         x |> foo ~blah:_ bar
       ]} *) ->
      (* We explicitly don't replace [_] as the applied function since there's no plan to
         support it in the partial application syntax. [[%eta]] should fill the role if
         needed. *)
      let used, args = maybe_replace_list_with_labels args in
      some_if_used ~used (Pexp_apply (func, args))
    | Pexp_tuple tuple
    (* {[
         x |> (_, bar);;
         x |> (~foo:_, bar)
       ]} *) ->
      let used, tuple = maybe_replace_list_with_labels tuple in
      some_if_used ~used (Pexp_tuple tuple)
    | Pexp_unboxed_tuple tuple
    (* {[
         x |> #(_, bar);;
         x |> #(~foo:_, bar)
       ]} *) ->
      let used, tuple = maybe_replace_list_with_labels tuple in
      some_if_used ~used (Pexp_unboxed_tuple tuple)
    | Pexp_record (fields, record)
    (* {[
         x |> { _ with foo; bar };;
         x |> { foo = _; bar }
       ]} *) ->
      let used, record =
        match record with
        | None -> `Not_used, None
        | Some record ->
          let used, record = maybe_replace record in
          used, Some record
      in
      let used, fields = maybe_replace_list_with_labels fields ~used in
      some_if_used ~used (Pexp_record (fields, record))
    | Pexp_record_unboxed_product (fields, record) ->
      (* {[
           x |> #{ _ with foo; bar };;
           x |> #{ foo = _; bar }
         ]} *)
      let used, record =
        match record with
        | None -> `Not_used, None
        | Some record ->
          let used, record = maybe_replace record in
          used, Some record
      in
      let used, fields = maybe_replace_list_with_labels fields ~used in
      some_if_used ~used (Pexp_record_unboxed_product (fields, record))
    | Pexp_field (record, field) (* [ x |> _.foo ] *) ->
      let used, record = maybe_replace record in
      some_if_used ~used (Pexp_field (record, field))
    | Pexp_unboxed_field (record, field) (* [ x |> _.#foo ] *) ->
      let used, record = maybe_replace record in
      some_if_used ~used (Pexp_unboxed_field (record, field))
    | Pexp_setfield (record, field, value)
    (* {[
         x |> (_.foo <- bar);;
         x |> (foo.bar <- _)
       ]} *) ->
      let used, record = maybe_replace record in
      let used, value = maybe_replace value ~used in
      some_if_used ~used (Pexp_setfield (record, field, value))
    | Pexp_constraint (e, ty, modes) (* [ x |> (_ : t) ] *) ->
      let used, e = maybe_replace e in
      some_if_used ~used (Pexp_constraint (e, ty, modes))
    | Pexp_coerce (e, ty_from, ty_to) (* [ x |> (_ :> t) ] *) ->
      let used, e = maybe_replace e in
      some_if_used ~used (Pexp_coerce (e, ty_from, ty_to))
    | Pexp_send (obj, label) (* [ x |> _#foo ] *) ->
      let used, obj = maybe_replace obj in
      some_if_used ~used (Pexp_send (obj, label))
    | Pexp_array (mut, exprs)
    (* {[
         x |> [| _; foo; bar |];;
         x |> [: _; foo; bar :]
       ]}
    *) ->
      let used, exprs =
        ListLabels.fold_left_map exprs ~init:`Not_used ~f:(fun used expr ->
          maybe_replace ~used expr)
      in
      some_if_used ~used (Pexp_array (mut, exprs))
    | Pexp_idx (Baccess_block (mut, e), accesses) ->
      (* {[
           x |> (.idx_imm(_).#x);;
           x |> (.idx_mut(_).#x)
         ]}
      *)
      let used, e = maybe_replace e in
      some_if_used ~used (Pexp_idx (Baccess_block (mut, e), accesses))
    | Pexp_override fields
    (* [ x |> {<foo = _>} ]
    *) ->
      let used, fields = maybe_replace_list_with_labels fields in
      some_if_used ~used (Pexp_override fields)
    | Pexp_borrow e ->
      (* [ x |> borrow_ _ ]
      *)
      let used, e = maybe_replace e in
      some_if_used ~used (Pexp_borrow e)
    | Pexp_assert e ->
      (* [ x |> assert _ ]
      *)
      let used, e = maybe_replace e in
      some_if_used ~used (Pexp_assert e)
    | Pexp_overwrite (e1, e2) ->
      (* {[
           x |> overwrite_ _ with bar;;
           x |> overwrite_ foo with _
         ]}
      *)
      let used, e1 = maybe_replace e1 in
      let used, e2 = maybe_replace e2 ~used in
      some_if_used ~used (Pexp_overwrite (e1, e2))
    | Pexp_stack _
    | Pexp_idx _
    | Pexp_setvar _
    | Pexp_open _
    (* Not recursing here because the open would change the interpretation *)
    | Pexp_let _
    | Pexp_function _
    | Pexp_match _
    | Pexp_ident _
    | Pexp_constant _
    | Pexp_unboxed_unit
    | Pexp_unboxed_bool _
    | Pexp_new _
    | Pexp_unreachable
    | Pexp_ifthenelse _
    | Pexp_variant (_, None)
    | Pexp_construct (_, None)
    | Pexp_try _
    | Pexp_sequence _
    | Pexp_while _
    | Pexp_for _
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_lazy _
    | Pexp_poly _
    | Pexp_object _
    | Pexp_newtype _
    | Pexp_pack _
    | Pexp_letop _
    | Pexp_extension _
    | Pexp_comprehension _
    | Pexp_quote _
    | Pexp_splice _ -> None
  in
  match result with
  | Some (Error message) -> Some (error_extensionf ~loc:expression.pexp_loc "%s" message)
  | None -> None
  | Some (Ok pexp_desc) ->
    Some
      { expression with
        pexp_desc =
          Ppxlib_jane.Shim.Expression_desc.to_parsetree ~loc:expression.pexp_loc pexp_desc
      }
;;

let add_errors_for_common_confusing_patterns expression =
  match
    Ppxlib_jane.Shim.Expression_desc.of_parsetree
      expression.pexp_desc
      ~loc:expression.pexp_loc
  with
  | Pexp_construct (constructor, Some argument)
  (* Better error messages for [|> Foo (2, _)] *) ->
    let count_holes list_with_labels =
      ListLabels.fold_left list_with_labels ~init:0 ~f:(fun count (_, argument) ->
        if is_hole argument then count + 1 else count)
    in
    let pexp_hole_count_in_arguments =
      match
        Ppxlib_jane.Shim.Expression_desc.of_parsetree
          argument.pexp_desc
          ~loc:argument.pexp_loc
      with
      | Pexp_tuple tuple -> count_holes tuple
      | Pexp_record (fields, _) -> count_holes fields
      | _ -> 0
    in
    (match pexp_hole_count_in_arguments with
     | 1 ->
       error_extensionf
         ~loc:{ expression.pexp_loc with loc_ghost = true }
         "The [_] in this expression was not translated by ppx_pipebang because it is \
          not at the top level of the expression. Consider writing [%s |> %s _] instead. \
          See %s for more info."
         (Pprintast.string_of_expression argument)
         (Longident.name constructor.txt)
         "documentation"
     | _ -> expression)
  | Pexp_apply (func, args) when is_hole func ->
    let error =
      error_extensionf
        ~loc:{ func.pexp_loc with loc_ghost = true }
        "This [_] was not translated by ppx_pipebang. When calling functions, [_] is \
         only supported for arguments."
    in
    { expression with
      pexp_desc =
        Ppxlib_jane.Shim.Expression_desc.to_parsetree
          (Pexp_apply (error, args))
          ~loc:expression.pexp_loc
    }
  | _ -> expression
;;

let expand (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_apply (_, [ (Nolabel, x); (Nolabel, y) ]) ->
    Some
      (match replace_hole_maybe y ~replacement:x with
       | Some replaced ->
         { e with
           pexp_desc = replaced.pexp_desc
         ; pexp_attributes = replaced.pexp_attributes @ e.pexp_attributes
         }
       | None ->
         let y = add_errors_for_common_confusing_patterns y in
         (match y with
          | { pexp_desc = Pexp_construct (id, None); _ } ->
            { e with pexp_desc = Pexp_construct (id, Some x) }
          | { pexp_desc = Pexp_apply (f, args); pexp_attributes = []; _ }
            when (* Do not inline |> as this would create applications with too many
                    arguments *)
                 not (is_pipe_application y) ->
            { e with pexp_desc = Pexp_apply (f, args @ [ Nolabel, x ]) }
          | _ -> { e with pexp_desc = Pexp_apply (y, [ Nolabel, x ]) }))
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
