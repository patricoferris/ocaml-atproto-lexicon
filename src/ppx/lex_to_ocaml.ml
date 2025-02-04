open Ppxlib
open Atproto_lexicon

open Ast_builder.Make (struct
  let loc = !Ast_helper.default_loc
end)

module Smap = Map.Make (String)

let camel_case s =
  let is_upper = function 'A' .. 'Z' -> true | _ -> false in
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      if is_upper c then (
        Buffer.add_char buf '_';
        Buffer.add_char buf (Char.lowercase_ascii c))
      else Buffer.add_char buf c)
    s;
  Buffer.contents buf

let keyword_prime = function "type" -> "type'" | "end" -> "end'" | s -> s
let lident_loc s = { txt = lident s; loc }
let loced s = { txt = s; loc }

let make_key s =
  let s = pstr_eval (pexp_constant (Pconst_string (s, loc, None))) [] in
  attribute ~name:(loced "key") ~payload:(PStr [ s ])

let ocamlise (s, is_module) =
  if is_module then (s, [])
  else
    let s' = keyword_prime (camel_case s) in
    if String.equal s s' then (s', []) else (s', [ make_key s ])

let nsid_resolution s =
  if String.starts_with ~prefix:"#" s then
    (String.sub s 1 (String.length s - 1), false)
  else
    match String.split_on_char '.' s with
    | _ :: _ :: rest ->
        List.map String.capitalize_ascii rest |> String.concat "." |> fun s ->
        (s ^ ".t", true)
    | _ -> failwith ("nsid resolution: " ^ s)

type type' =
  | Declaration of type_declaration
  | Core of core_type
  | Record of label_declaration list

let record ~name lbls =
  type_declaration ~private_:Public ~name:(loced name) ~kind:(Ptype_record lbls)
    ~params:[] ~cstrs:[] ~manifest:None

let td_of_core_type ~name c =
  type_declaration ~private_:Public ~name:(loced name) ~kind:Ptype_abstract
    ~params:[] ~cstrs:[] ~manifest:(Some c)

let td_or_core id c =
  match id with
  | Some name -> Declaration (td_of_core_type ~name c)
  | None -> Core c

let concrete_to_core_type' = function
  | `Boolean -> ptyp_constr (lident_loc "bool") []
  | `Integer _ -> ptyp_constr (lident_loc "int") []
  | `String _ -> ptyp_constr (lident_loc "string") []
  | `Bytes _ -> ptyp_constr (lident_loc "bytes") []
  | `Cid_link -> ptyp_constr (lident_loc "link") []
  | `Blob -> ptyp_constr (lident_loc "blob") []
  | `Null -> ptyp_constr (lident_loc "null") []

let def_of_json json =
  Jsont_bytesrw.decode_string Atproto_lexicon.Definition.jsont
    (Jsont_bytesrw.encode_string Jsont.json json |> Result.get_ok)
  |> function
  | Ok v -> v
  | Error s ->
      Format.eprintf "Error:\n%s\nJSON:%a" s Jsont.pp_json json;
      invalid_arg "Failed to parse container type"

let rec concrete_to_core_type ?id ~required v =
  if required then concrete_to_core_type' v |> td_or_core id
  else
    ptyp_constr (lident_loc "option") [ concrete_to_core_type' v ]
    |> td_or_core id

and container_to_type ?id (container : Definition.container) =
  match container with
  | `Object o -> (
      let props = o.properties in
      let required = o.required in
      let labels =
        Smap.fold
          (fun label json acc ->
            let required = List.mem label required in
            let def = def_of_json json in
            match def_to_type ~required def with
            | Core type_ ->
                let name, attrs = ocamlise (label, false) in
                let lbl =
                  label_declaration ~name:(loced name) ~mutable_:Immutable
                    ~type_
                in
                { lbl with pld_attributes = attrs } :: acc
            | Declaration _ | Record _ ->
                failwith "Unexpected type declaration/record!")
          props []
      in
      match id with
      | Some name -> Declaration (record ~name labels)
      | None -> Record labels)
  | `Array arr -> (
      let v = arr.items |> def_of_json in
      match def_to_type v with
      | Core c -> Core (ptyp_constr (lident_loc "list") [ c ])
      | _ -> failwith "TODO: arrays")
  | _ -> failwith "Unsupported container type"

and primary_to_type ~required:_ (p : Definition.primary) =
  match p with
  | `Record r -> container_to_type (`Object r.record)
  | _ -> assert false

and union_to_type (u : Definition.union) =
  let variant_names =
    List.map
      (fun s ->
        let lbl =
          String.split_on_char '.' s |> List.rev |> List.hd
          |> String.capitalize_ascii
        in
        let type_, _ = ocamlise @@ nsid_resolution s in
        {
          prf_desc =
            Rtag (loced lbl, false, [ ptyp_constr (lident_loc type_) [] ]);
          prf_loc = loc;
          prf_attributes = [];
        })
      u.refs
  in
  ptyp_variant variant_names Closed None

and meta_to_type ~required:_ (v : Definition.meta) =
  match v with
  | `Ref r ->
      Core
        (ptyp_constr (lident_loc (fst @@ ocamlise (nsid_resolution r.ref'))) [])
  | `Union u -> Core (union_to_type u)
  | _ -> assert false

and def_to_type ?(required = true) (def : Atproto_lexicon.Definition.t) : type'
    =
  match def.type' with
  | #Definition.concrete as v -> concrete_to_core_type ~required v
  | #Definition.container as c -> container_to_type c
  | #Definition.primary as p -> primary_to_type ~required p
  | #Definition.meta as v -> meta_to_type ~required v

let defs_to_types ~id:_ defs =
  let str t = pstr_type Recursive t in
  let folder name def acc =
    let name =
      match name with "main" -> "t" | name -> fst @@ ocamlise (name, false)
    in
    match def_to_type def with
    | Declaration d -> d :: acc
    | Record lbls -> record ~name lbls :: acc
    | Core c -> td_of_core_type ~name c :: acc
  in
  Smap.fold folder defs [] |> str
