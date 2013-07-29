
type compile_options = {
  src : (string * string) list ;
  output : string
}

type compile_result = {
  stdout : string ;
  bytecode : string
}

open Webworker

(* BEWARE : terminate worker when close workspace *)

let string_to_blob s =
  let open Typed_array in
  let a = jsnew int8Array(String.length s) in
  for i = 0 to (String.length s) - 1 do
    set a i (int_of_char s.[i])
  done;
  let a = [| a##buffer |] in 
  let a = Js.Unsafe.fun_call (Js.Unsafe.variable "new Blob")
    [| Js.Unsafe.inject a ; 
       Js.Unsafe.inject
         (Js.Unsafe.variable 
            "{type: \"application/octet-stream;charset=utf-8\"}")
    |] in
  a

let worker = jsnew webWorker(Js.string "ocamlc.js")

let compile callback opts =
  let msg = Json.output opts in
  worker##onmessage <- (fun ev ->
    let data: compile_result = Json.unsafe_input ev##data in
    let blob = string_to_blob data.bytecode in
    Ace_utils.console_debug blob;
    Js.Unsafe.fun_call (Js.Unsafe.variable "saveAs")
      [| Js.Unsafe.inject blob; Js.Unsafe.inject (Js.string opts.output) |]
  );
  worker##postMessage(msg)

let _ =
  (Js.Unsafe.coerce Dom_html.window)##compile <- Js.wrap_callback compile
