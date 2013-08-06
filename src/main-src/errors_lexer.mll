{

  open Errors_format

  let errors = ref []

  let error_line_column file line start _end =
    let nb = List.length !errors in
    errors := { file; line; chars = (start, _end) } :: !errors;
    let file = Format.sprintf "<span \
    style=\"color:red;font-weight:bold\">%s</span>" file in
    let line = Format.sprintf "<span style=\"color:green\">%d</span>" line in
    let res = Format.sprintf "File \"%s\", line %s, characters %d-%d:"
      file line start _end in
    Format.sprintf "<a id=\"error%d\" class=\"error-link\">%s</a>@." nb res
      
  let error_line file line =
    let nb = List.length !errors in
    (* (-1, -1) doit être interprété comme la ligne entière *)
    errors := { file; line; chars = (-1, -1) } :: !errors;
    let file = Format.sprintf "<span \
    style=\"color:red;font-weight:bold\">%s</span>" file in
    let line = Format.sprintf "<span style=\"color:green\">%d</span>" line in
    let res =
      Format.sprintf "File \"%s\", line %s:"
        file line in
    Format.sprintf "<a id=\"error%d\" class=\"error-link\">%s</a>@." nb res
    
}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let number = digit+ ('.')? digit*
let char = (alpha | digit | '_' | '-' | '/' | '.')
let ident = (alpha | '_') (alpha | '_' | digit)*
let space = (' ' | '\t' | '\r' | '\n')


rule line = parse
  | "File \"" (ident".ml"['i']? as file)
      "\", line " (integer as line) 
      ", characters " (integer as start) "-" (integer as _end) ":\n"
      { error_line_column 
          file (int_of_string line) 
          (int_of_string start) 
          (int_of_string _end) }
  | "File \"" (ident".ml"['i']? as file)
      "\", line " (integer as line)  ":\n"
      { error_line file (int_of_string line) } 
  | eof { raise End_of_file }
  | "\n" { "\n" }
  | _ as s { Char.escaped s }

{
  
  let parse_compile_output output =
    errors := [];
    let lexbuf = Lexing.from_string output in
    let buff = Buffer.create 501 in
    try
      while true do
        let read = line lexbuf in
        Buffer.add_string buff read
      done;
      Buffer.contents buff, !errors (* Unreachable *)
    with
        End_of_file -> Buffer.contents buff, !errors 

}
