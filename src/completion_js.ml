
let rec print_words = function
  | [] -> ()
  | w :: l -> Ace_utils.console_log w; print_words l


(** Js bindings of OCaml functions **)


let compute_completions w =
  let w = Js.to_string w in
  Autocomplete.compute_completions w

let next_completion () =
  try
    Js.string (Autocomplete.next_completion () )
  with
      _ -> Js.string ""

let add_words_from_string str =
  let str = Js.to_string str in
  Autocomplete.create_from_string str
  


let list_to_js_array l =
  let rec convert acc = function 
    | [] -> acc
    | w :: l -> convert ((Js.string w) :: acc) l
  in 
  let a = Array.of_list (convert [] l) in
  Js.array a

let find_completion_js w =
  let w = Js.to_string w in
  let l = Autocomplete.set_to_list (Autocomplete.find_completion w) in
  (* print_words l *)
  list_to_js_array l

let new_word_from_js w =
  Autocomplete.add_word (Js.to_string w)



let _ = 
  (Js.Unsafe.coerce Dom_html.window)##complete <- find_completion_js;
  (Js.Unsafe.coerce Dom_html.window)##newWord <- new_word_from_js;
  (Js.Unsafe.coerce Dom_html.window)##computeCompletions <-
  compute_completions;
  (Js.Unsafe.coerce Dom_html.window)##nextCompletion <- next_completion
