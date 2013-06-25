(**
   Tiny lexing module to retrieve potentials idents
**)

{
  open Completion_data
  
  exception Eof_exception

  let keywords = ["let", (); 
                  "and", ();
                  "in", (); 
                  "match", (); 
                  "with", (); 
                  "function", ();
                  "begin", (); 
                  "end", (); 
                  "try", ();
                  "for", ();
                  "type", ();
                  "while", ();
                  "do", ();
                  "if", ();
                  "then", ();
                  "else", ();
                  "done", ();
                  "failwith", (); 
                  "raise", (); 
                  "assert", ();
                  "open", ();
                  "rec", () ]

  let opening_keywords = ["let"; "begin"; "do"]

  let closing_keywords = ["in"; "end"; "done"]


  let keywords_htbl = 
    let h = Hashtbl.create 19 in
    List.iter (fun s -> Hashtbl.add h (fst s) (snd s)) keywords;
    h

}

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let number = digit+ ('.')? digit*
let char = (alpha | digit | '_' | '-' | '/' | '.')
let ident = (alpha | '_') (alpha | '_' | digit)*
let space = (' ' | '\t' | '\r' | '\n')


rule global = parse
  | '_' { }

  | ("let"|"and") (space+ "rec")? space+ (ident as s) space+
      { 
        if not (Hashtbl.mem keywords_htbl s) then
          new_word s
      }
  | eof 
      { raise Eof_exception }
  | "(*"
      { 
        comment lexbuf 
      }
  | _
      { }

and comment = parse
  | "*)" { }
  | "(*" { comment lexbuf; comment lexbuf }
  | _ { comment lexbuf }

{
  let parse_channel ch =
    let lexbuf = Lexing.from_channel ch in
    try
      while true do
        global lexbuf
      done
    with
        _ -> ()
  
  let parse_string str =
    let lexbuf = Lexing.from_string str in
    try
      while true do
        global lexbuf
      done
    with
        _ -> ()
}
      
