
type compile_options = {
  project : string;
  src : (string * string) list ;
  output : string
}

type compile_result = {
  initial_proj : string;
  stdout : string ;
  exec : string ;
  bytecode : string;
  code: int
}

open Webworker

(* BEWARE : terminate worker when close workspace *)

(* Reference sur le worker : work around pour éviter de créer le worker dans la
   fonction compile -> de cette manière, il est créé dès la destruction de
   l'ancien et on évite ainsi que le temps de "création" soit visible pour
   l'utilisateur *)

let worker = ref (jsnew webWorker(Js.string "ocamlc.js")) 

let compile callback opts =
  let msg = Json.output opts in
  (!worker)##onmessage <- (fun ev ->
    let data: compile_result = Json.unsafe_input ev##data in
    callback data;
    (!worker)##terminate ();
    worker := jsnew webWorker(Js.string "ocamlc.js"));
  (!worker)##postMessage(msg)


let main () =
  (Js.Unsafe.coerce Dom_html.window)##compile <- Js.wrap_callback compile
