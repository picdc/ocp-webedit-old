
open Conftypes

exception Bad_conf_file


let split_first str c =
  let pos = String.index str c in
  let s1 = String.sub str 0 pos in
  let s2 = String.sub str (pos+1) (String.length str - pos-1) in
  s1, s2 

let split str c =
  let rec aux acc str =
    let pos = 
      try String.index str c
      with Not_found -> -1 in
    if pos <> -1 then
      let el = String.sub str 0 pos in
      let next = String.sub str (pos+1) (String.length str - pos-1) in
      aux (el::acc) next
    else str::acc
  in
  List.rev (aux [] str)
 

let parse_to_conf str =
  let get_var line =
    try split_first line '='
    with Not_found -> raise Bad_conf_file
  in 
  List.rev (List.fold_left
              (fun acc line -> if line <> "" then (get_var line)::acc
                else acc) [] (split str '\n'))

    
exception Unknown_conf_var of string
    
let parse_to_compile_conf conf =
  let files = ref [] in
  let output = ref "" in
  let step (key, value) =
    match key with
      | "output" -> output := value
      | "files" -> files := split value ','
      | _ -> raise (Unknown_conf_var key)
  in
  List.iter step conf;
  { files = !files; output = !output }


let generate_of_conf conf =
  let buf = Buffer.create 503 in
  List.iter (fun (key,value) ->
    let s = Format.sprintf "%s=%s\n" key value in
    Buffer.add_string buf s) conf;
  Buffer.contents buf


let generate_of_compile_conf cconf =
  let files = List.filter (fun s -> s <> "") cconf.files in 
  let files = String.concat "," files in
  ["files", files ; "output", cconf.output]

