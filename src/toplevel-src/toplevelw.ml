
let split_primitives p =
  let len = String.length p in
  let rec split beg cur =
    if cur >= len then []
    else if p.[cur] = '\000' then
      String.sub p beg (cur - beg) :: split (cur + 1) (cur + 1)
    else
      split beg (cur + 1) in
  Array.of_list(split 0 0)

class type global_data = object
  method toc : (string * string) list Js.readonly_prop
  method compile : (string -> string) Js.writeonly_prop
end

external global_data : unit -> global_data Js.t = "caml_get_global_data"

let g = global_data ()

let _ =
  let toc = g##toc in
  let prims = split_primitives (List.assoc "PRIM" toc) in

  let compile s =
    let output_program = Driver.from_string prims s in
    let b = Buffer.create 100 in
    output_program (Pretty_print.to_buffer b);
    Buffer.contents b
  in
  g##compile <- compile (* XXX HACK! *)





let onmessage f =
  (Js.Unsafe.coerce Dom_html.window)##onmessage <- Js.wrap_callback f

let postMessage msg =
  (Js.Unsafe.coerce Dom_html.window)##postMessage(msg)
  


type toplevel_worker_msg = Reset | Eval of string
type toplevel_worker_res = { eval : string ; output : string }


exception End_of_input
let eval str =
  let buf = Buffer.create 503 in
  let ppf =
    let b = Buffer.create 80 in
    Format.make_formatter
      (fun s i l -> Buffer.add_substring b s i l)
      (fun () -> Buffer.add_buffer buf b; Buffer.clear b) in
  let str = match str with 
    | "" -> ""
    | _ -> str ^ ";;" in
  let lb = Lexing.from_string str in
  begin try
    let phr =
      try !Toploop.parse_toplevel_phrase lb
      with End_of_file -> raise End_of_input in
    ignore(Toploop.execute_phrase true ppf phr)
  with
      End_of_input -> ()
    | exn -> Errors.report_error ppf exn
  end;
  let eval = Buffer.contents buf in
  let output = Js.to_string (Js.Unsafe.coerce Dom_html.window)##stdout_buf in
  (Js.Unsafe.coerce Dom_html.window)##stdout_buf <- Js.string "";
  postMessage (Json.output ({ eval ; output }))



let _ =
  onmessage (fun ev ->
    match Json.unsafe_input ev##data with
        Reset -> Toploop.initialize_toplevel_env ()
      | Eval str -> eval str)
