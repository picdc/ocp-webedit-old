
let byte_to_hex i =
  let x1 = i / 16 in
  let x2 = i mod 16 in
  let aux i =
    match i with
    | i when i < 10 -> string_of_int i
    | 10 -> "a" | 11 -> "b" | 12 -> "c" | 13 -> "d" | 14 -> "e" | 15 -> "f"
    | _ -> assert false
  in
  (aux x1)^(aux x2)

let _ =
  let inc = open_in "std_exit.cmo" in
  let buf = Buffer.create 503 in
  let rec aux () =
    try
      let c = input_byte inc in
      begin
	match c with
	(* | i when i >= 32 && i <= 122 && i <> 34 && i <> 39 -> *)
	(*   Buffer.add_char buf (char_of_int i) *)
	| i -> 
	  Buffer.add_string buf ("\\x"^(byte_to_hex i))
      end;
      aux ()
    with _ -> Buffer.contents buf
  in
  print_endline (aux ())