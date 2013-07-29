
type compile_options = {
  src : (string * string) list ;
  output : string
}

type compile_result = {
  stdout : string ;
  exec : string ;
  bytecode : string;
  code: int
}

open Webworker

(* BEWARE : terminate worker when close workspace *)

let worker = jsnew webWorker(Js.string "ocamlc.js")

let compile callback opts =
  let msg = Json.output opts in
  worker##onmessage <- (fun ev ->
    let data: compile_result = Json.unsafe_input ev##data in
    callback data);
  worker##postMessage(msg)

let _ =
  (Js.Unsafe.coerce Dom_html.window)##compile <- Js.wrap_callback compile
