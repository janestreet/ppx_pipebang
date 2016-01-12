open! StdLabels
open Ppx_core.Std
open Parsetree

[@@@metaloc loc]

let map = object(self)
  inherit Ast_traverse.map as super

  method! expression e =
    let loc = e.pexp_loc in
    match e.pexp_desc with
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident ("|!" | "|>"); _ }; _ },
                  [("", x); ("", y)]) -> begin
        let x = self#expression x in
        let y = self#expression y in
        match y with
        | { pexp_desc = Pexp_construct (id, None); _ } ->
          { y with pexp_desc = Pexp_construct (id, Some x) }
        | { pexp_desc = Pexp_apply (f, args); pexp_attributes = []; _ } ->
          { e with pexp_desc = Pexp_apply (f, args @ [("", x)]) }
        | _ ->
          { e with pexp_desc = Pexp_apply (y, [("", x)]) }
      end
    | Pexp_ident { txt = Lident ("|!" | "|>" as s); _ } ->
      Location.raise_errorf ~loc "%s must be applied to two arguments" s
    | _ -> super#expression e
end

let () =
  Ppx_driver.register_transformation "pipebang"
    ~impl:map#structure
    ~intf:map#signature
;;
