
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
  worker##postMessage(Js.string opts.output)

let _ =
  (Js.Unsafe.coerce Dom_html.window)##compile <- Js.wrap_callback compile
