let lex_to_ocaml (l : Atproto_lexicon.t) =
  let defs = l.defs in
  let types = Lex_to_ocaml.defs_to_types ~id:l.id defs in
  Ppxlib.Pprintast.structure_item Format.std_formatter types

let () =
  let lexicon_s = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  match Atproto_lexicon.of_string lexicon_s with
  | Error s -> invalid_arg s
  | Ok lex -> lex_to_ocaml lex
