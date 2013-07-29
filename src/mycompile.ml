
type bytecode = string 

type compile_options = {
  src : (string * string) list ;
  output : string
}

open Webworker

(* BEWARE : terminate worker when close workspace *)

let worker = jsnew webWorker(Js.string "ocamlc.js")

let compile callback opts =
  worker##onmessage <- (fun ev -> print_endline (Js.to_string ev##data));
  worker##postMessage(Js.string (
    let res = List.fold_left 
      (fun acc (name, content) ->
      Format.sprintf "%s--- %s ---\n%s\n@." acc name content)
      "Files sent to compiler@."
      opts.src 
    in
    Format.sprintf "Output created : %s @.@.%s" opts.output res
  ))

let _ =
  (Js.Unsafe.coerce Dom_html.window)##compile <- Js.wrap_callback compile
