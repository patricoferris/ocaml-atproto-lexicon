module Smap = Map.Make (String)

let test_post () =
  let post =
    match Atproto_lexicon.of_string Examples.post with
    | Ok v -> v
    | Error s -> Alcotest.fail s
  in
  Alcotest.(check int) "same lexicon" 1 post.lexicon;
  let defs = post.defs in
  Alcotest.(check int) "same num defs" 4 (Smap.cardinal defs);
  let main = (Smap.find "main" defs).type' in
  match main with
  | `Record r ->
      let key = r.key in
      Alcotest.(check string) "same key" "tid" key
  | _ -> Alcotest.fail "Expected a record for main definition"

let () =
  Alcotest.run "atproto-lexicon"
    [ ("lexicons", [ Alcotest.test_case "post" `Quick test_post ]) ]
