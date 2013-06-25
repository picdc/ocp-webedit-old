(**
 ocamlc -I ~/.opam/4.00.1/lib/re re.cma re_emacs.cma re_str.cma -o autocomplete autocomplete.ml && ./autocomplete 
**)


open Completion_data

(** (* Creation functions *) **)

let reset_env () =
  actual_env := empty_env ()

let add_word = new_word

let create_from_string str =
  Completion_lexer.parse_string str


let create_from_channel ch =
  Completion_lexer.parse_channel ch

(** Utils functions **)

let set_to_list s =
  List.rev (Words.fold (fun elt l -> elt :: l) s [])

let set_to_array s =
  let a = Array.make (Words.cardinal s) "" in
  ignore (Words.fold (fun elt i -> a.(i) <- elt; i+1) s 0);
  a

let print_word_from_set s =
  let rec print = function
    | [] -> []
    | w :: l -> Format.printf "%s@." w; print l
  in
  print (set_to_list s)

(** Functions to compute completion **)

let find_completion w =
  origin := w;
  let re = "^" ^ w ^ ".*" in
  let re = Re_str.regexp re in
  (* let re = Re.compile re in *)
  
  let rec step env acc =
    Words.fold
      (fun s acc ->
        (* Format.printf "%s@." s; *)
        if  Re_str.string_match re s 0 then 
          begin
            Words.add s acc
          end
        else acc)
      env
      acc
  in
  step !words Words.empty

let compute_completions w =
  let words = find_completion w in
  let words = set_to_array words in
  completions := words;
  actual_index := 0

exception No_completion

let next_completion () =
  if Array.length !completions = 0 then !origin
  else
    let n = !completions.(!actual_index) in
    actual_index := (!actual_index + 1) mod Array.length !completions;
    n
