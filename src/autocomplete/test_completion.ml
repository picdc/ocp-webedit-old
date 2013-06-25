open Autocomplete

let _ =
  reset_env ();
  add_word "get_use";
  add_word "get_i";
  add_word "match_stg";
  let f = open_in "indentBlock.ml" in
  reset_env ();
  create_from_channel f;
  compute_completions "un";
  let n = next_completion () in
  Format.printf "%s@." n;
  let n = next_completion () in
  Format.printf "%s@." n;
  let n = next_completion () in
  Format.printf "%s@." n
