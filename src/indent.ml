
let get_last_anchor row =
  if row = 0 then 0
  else
    let count = ref 0 in
    let rec search row =
      if row = 0 then 0
      else begin
	let tokens = Ace.EditSession.getTokens
	  (Ace.Editor.getSession (Ace_utils.editor ())) row in
	let is_done = Array.fold_left (fun b t ->
	  if not b then
	    begin
	      match Ace.Token._type t with
	      | "keyword" ->
		begin
		  match Ace.Token.value t with
		  | "in" | "end" | "done" ->
		    (incr count;
		     false)
		  | "let" | "begin" | "do" ->
		    (decr count;
		     if !count <= 0 then true
		     else false)
		  (* | "match" when (!count = 0) -> true *)
		  | _ -> false
		end
	      | _ -> false	
	    end
	  else true)
	  false tokens
	in
	if is_done then row
	else search (row-1)
      end
    in
    search (row-1)


let kind_num result = IndentPrinter.Numeric
  (fun n -> result := n)

let kind_multinum result = IndentPrinter.Numeric
  (fun n -> Queue.add n result)

let call_ocp_indent kind text =
  let char_left = ref (String.length text) in
  let pos_str = ref 0 in

  let f_config = IndentConfig.default in
  let f_in_lines n = true in
  let output = {
    IndentPrinter.
    debug = false;
    config = f_config;
    in_lines = f_in_lines;
    indent_empty = true;
    kind; }
  in

  let reader buf n =
    (* Le nombre maximum de caractère à traiter pendant cette passe *)
    let n =
      if n > !char_left then !char_left
      else n
    in
    (* Traitement et mise à jour *)
    String.blit text !pos_str buf 0 n;
    char_left := !char_left - n;
    pos_str := !pos_str + n;
    n
  in

  let state = IndentPrinter.initial in
  let stream = Nstream.make reader in
  ignore (IndentPrinter.stream output ~resume:state stream)

let get_indent_size (line: string) : int =
  let size = String.length line in
  let tab_size = Ace.EditSession.getTabSize 
    (Ace.Editor.getSession (Ace_utils.editor ())) in
  let rec aux i =
    if i >= size then i
    else
      (let c = String.get line i in
       if c = ' ' then aux (i+1)
       else if c = '\t' then aux (i+tab_size)
       else i)
  in
  aux 0

let get_indent_line row : Js.js_string Js.t =
  let last_anchor = get_last_anchor row in
  let doc = Ace.EditSession.getDocument
    (Ace.Editor.getSession (Ace_utils.editor ())) in
  let add =
    if last_anchor = 0 then 0
    else get_indent_size (Ace.Document.getLine doc last_anchor) in
  let text = Ace_utils.get_lines doc last_anchor row in
  let res = ref 0 in
  call_ocp_indent (kind_num res) text;
  Js.string (String.make (!res+add) ' ')

let replace_indent row n =
  let doc = Ace.EditSession.getDocument
    (Ace.Editor.getSession (Ace_utils.editor ())) in
  let size = get_indent_size (Ace.Document.getLine doc row) in
  let range = Ace.Range.range row 0 row size in
  let new_indent = String.make n ' ' in
  Ace.Document.replace doc range new_indent

let indent_line row =
  let doc = Ace.EditSession.getDocument
    (Ace.Editor.getSession (Ace_utils.editor ())) in
  let last_anchor = get_last_anchor row in
  let add =
    if last_anchor = 0 then 0
    else get_indent_size (Ace.Document.getLine doc last_anchor) in
  let text = Ace_utils.get_lines doc last_anchor row in
  let res = ref 0 in
  call_ocp_indent (kind_num res) text;
  replace_indent row (!res+add)
      
let indent_region row_start row_end  =
  if row_start = row_end then indent_line row_start
  else
    (let res = Queue.create () in
     let rec search_anchor anc =
       if anc = 0 then 0
       else if row_start <= anc then search_anchor (get_last_anchor anc)
       else anc in

     let last_anchor = search_anchor (get_last_anchor row_end) in
     let doc = Ace.EditSession.getDocument
       (Ace.Editor.getSession (Ace_utils.editor ())) in
     let add =
       if last_anchor = 0 then 0
       else get_indent_size (Ace.Document.getLine doc last_anchor) in
     let text = Ace_utils.get_lines doc last_anchor row_end in
     (* Ace_utils.console_debug (Js.string text); *) (* DEBUG *)
     call_ocp_indent (kind_multinum res) text;

     ignore (Queue.fold (fun row n ->
       if row >= row_start && row <= row_end then
	 replace_indent row (n+add);
       row + 1
     ) last_anchor res))

let _ =
  (Js.Unsafe.coerce Dom_html.window)##getIndentLine <- Js.wrap_callback
    get_indent_line;
  (Js.Unsafe.coerce Dom_html.window)##indentLine <- Js.wrap_callback
    indent_line;
  (Js.Unsafe.coerce Dom_html.window)##indentRegion <- Js.wrap_callback
    indent_region

 (** ocamlc -I ocp-indent-src/ ocp_indent.cma -o testingindent.byte indent.ml && ./testingindent.byte **)
  (* let file = open_in "ocp-indent-src/indentBlock.ml" in *)
  (* let buf = Buffer.create 5003 in *)
  (* try *)
  (*   while true do *)
  (*     Buffer.add_string buf (input_line file); *)
  (*     Buffer.add_string buf "\n" *)
  (*   done *)
  (* with End_of_file -> *)
  (*   begin *)
  (*     let res = ref 0 in *)
  (*     print_endline (string_of_float (Sys.time ())); *)
  (*     call_ocp_indent (kind_num res) (Buffer.contents buf) (50,60); *)
  (*     print_endline (string_of_float (Sys.time ())) *)
  (*   end *)
