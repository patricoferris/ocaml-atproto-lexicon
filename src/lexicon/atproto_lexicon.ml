(* Definitions *)
Jsont.rec'

module Definition = struct
  type 'a definition = { type' : 'a; description : string option }
  and record = { key : string; record : object' }

  and object' = {
    properties : Jsont.json Map.Make(String).t;
    required : string list;
    nullable : string list;
  }

  and string' = {
    format : string option;
    max_length : int option;
    min_length : int option;
    known_values : string list;
  }

  and int' = { minimum : int option; maximum : int option }
  and ref' = { ref' : string }
  and union = { refs : string list; closed : bool }
  and bytes' = { bytes_max_length : int option; bytes_min_length : int option }
  and params = { required : string list; max_size : int option }
  and array = { items : Jsont.json }
  and query = { params : object' definition option }
  and procedure = query
  and subscription = { params : object' definition option }

  and concrete =
    [ `Null
    | `Boolean
    | `Integer of int'
    | `String of string'
    | `Bytes of bytes'
    | `Cid_link
    | `Blob ]

  and container = [ `Array of array | `Object of object' | `Params of params ]
  and meta = [ `Token | `Ref of ref' | `Union of union | `Unknown ]

  and primary =
    [ `Record of record
    | `Query of query
    | `Procedure of procedure
    | `Subscription of subscription ]

  type t = [ concrete | container | primary | meta ] definition

  let make_record key record = { key; record }

  let make_object properties required nullable =
    { properties; required; nullable }

  let make_string format max_length min_length known_values =
    { format; max_length; min_length; known_values }

  let make_params required max_size = { required; max_size }

  let make_bytes bytes_max_length bytes_min_length =
    { bytes_max_length; bytes_min_length }

  let make_array items = { items }
  let make_int minimum maximum = { minimum; maximum }
  let make_ref ref' = { ref' }
  let make_union closed refs = { refs; closed }

  let concrete_assoc =
    [
      ("null", `Null);
      ("boolean", `Boolean);
      ("integer", `Integer);
      ("string", `String);
      ("bytes", `Bytes);
      ("cid-link", `Cid_link);
      ("blob", `Blob);
    ]

  let record r = `Record r
  let query q = `Query q
  let procedure p = `Procedure p
  let subscription s = `Subscription s
  let object' o = `Object o
  let string s = `String s
  let bytes b = `Bytes b
  let params p = `Params p
  let array a = `Array a
  let int i = `Integer i
  let ref' r = `Ref r
  let union u = `Union u

  module Nsid = struct
    type t = string
  end

  let make ?description type' = { type'; description }
  let type' t = t.type'
  let as_def type' description = { type'; description }

  let finish_def map =
    map
    |> Jsont.Object.opt_mem "description" Jsont.string
    |> Jsont.Object.finish

  let string_jsont =
    Jsont.Object.map ~kind:"String" make_string
    |> Jsont.Object.opt_mem "format" Jsont.string
    |> Jsont.Object.opt_mem "maxLength" Jsont.int
    |> Jsont.Object.opt_mem "minLength" Jsont.int
    |> Jsont.Object.mem "knownValues" ~dec_absent:[] Jsont.(list string)
    |> Jsont.Object.finish

  let bytes_jsont =
    Jsont.Object.map ~kind:"Bytes" make_bytes
    |> Jsont.Object.opt_mem "maxLength" Jsont.int
    |> Jsont.Object.opt_mem "minLength" Jsont.int
    |> Jsont.Object.finish

  let int_jsont =
    Jsont.Object.map ~kind:"Integer" make_int
    |> Jsont.Object.opt_mem "minimum" Jsont.int
    |> Jsont.Object.opt_mem "maximum" Jsont.int
    |> Jsont.Object.finish

  let ref_jsont =
    Jsont.Object.map ~kind:"Ref" make_ref
    |> Jsont.Object.mem "ref" Jsont.string
    |> Jsont.Object.finish

  let union_jsont =
    Jsont.Object.map ~kind:"Union" make_union
    |> Jsont.Object.mem "closed" ~dec_absent:false Jsont.bool
    |> Jsont.Object.mem "refs" Jsont.(list string)
    |> Jsont.Object.finish

  let rec jsont =
    lazy
      (let make' description type' = { type'; description } in
       let record =
         Jsont.Object.Case.map "record" (Lazy.force record_jsont) ~dec:record
       in
       let object' =
         Jsont.Object.Case.map "object" (Lazy.force object_jsont) ~dec:object'
       in
       let string = Jsont.Object.Case.map "string" string_jsont ~dec:string in
       let bytes = Jsont.Object.Case.map "bytes" bytes_jsont ~dec:bytes in
       let int = Jsont.Object.Case.map "integer" int_jsont ~dec:int in
       let ref' = Jsont.Object.Case.map "ref" ref_jsont ~dec:ref' in
       let union = Jsont.Object.Case.map "union" union_jsont ~dec:union in
       let array =
         Jsont.Object.Case.map "array" (Lazy.force array_jsont) ~dec:array
       in
       let enc_case = function
         | `Record r -> Jsont.Object.Case.value record r
         | `Object o -> Jsont.Object.Case.value object' o
         | `String s -> Jsont.Object.Case.value string s
         | `Bytes b -> Jsont.Object.Case.value bytes b
         | `Array a -> Jsont.Object.Case.value array a
         | `Integer i -> Jsont.Object.Case.value int i
         | `Ref a -> Jsont.Object.Case.value ref' a
         | `Union a -> Jsont.Object.Case.value union a
         | _ -> assert false
       in
       let primary_cases = Jsont.Object.Case.[ make record ] in
       let container_cases = Jsont.Object.Case.[ make object'; make array ] in
       let concrete_cases =
         Jsont.Object.Case.[ make string; make bytes; make int ]
       in
       let meta_cases = Jsont.Object.Case.[ make union; make ref' ] in
       let cases =
         primary_cases @ container_cases @ concrete_cases @ meta_cases
       in
       Jsont.Object.map ~kind:"Definition" make'
       |> Jsont.Object.opt_mem "description" Jsont.string
       |> Jsont.Object.case_mem "type" Jsont.string ~enc:type' ~enc_case cases
       |> Jsont.Object.finish)

  and record_jsont : record Jsont.t Lazy.t =
    lazy
      (Jsont.Object.map ~kind:"Record" make_record
      |> Jsont.Object.mem "key" Jsont.string
      |> Jsont.Object.mem "record" (Lazy.force object_jsont)
      |> Jsont.Object.finish)

  and object_jsont : object' Jsont.t Lazy.t =
    lazy
      (Jsont.Object.map ~kind:"Object" make_object
      |> Jsont.Object.mem "properties"
           (Jsont.Object.as_string_map Jsont.json_object)
      |> Jsont.Object.mem "required" ~dec_absent:[] Jsont.(list string)
      |> Jsont.Object.mem "nullable" ~dec_absent:[] Jsont.(list string)
      |> Jsont.Object.finish)

  and array_jsont =
    lazy
      (Jsont.Object.map ~kind:"Array" make_array
      |> Jsont.Object.mem "items" Jsont.json
      |> Jsont.Object.finish)

  let jsont : t Jsont.t = Lazy.force jsont
end

(* Lexicon File *)
type t = {
  lexicon : int;
  id : Definition.Nsid.t;
  revision : int option;
  description : string option;
  defs : Definition.t Map.Make(String).t;
}

let lexicon t = t.lexicon
let id t = t.id
let revision t = t.revision
let description t = t.description
let defs t = t.defs

let make ?(lexicon = 1) ?revision ?description ~id defs =
  { lexicon; id; revision; description; defs }

let jsont : t Jsont.t =
  let make' lexicon id revision description defs =
    make ~lexicon ?revision ?description ~id defs
  in
  Jsont.Object.map ~kind:"Lexicon" make'
  |> Jsont.Object.mem "lexicon" Jsont.int ~enc:lexicon
  |> Jsont.Object.mem "id" Jsont.string ~enc:id
  |> Jsont.Object.opt_mem "revision" Jsont.int ~enc:revision
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:description
  |> Jsont.Object.mem "defs"
       (Jsont.Object.as_string_map Definition.jsont)
       ~enc:defs
  |> Jsont.Object.finish

let of_string = Jsont_bytesrw.decode_string jsont
let to_string = Jsont_bytesrw.encode_string jsont
let of_reader = Jsont_bytesrw.decode jsont
