
type config = { mutable editor : Ace.editor Js.t option ;
		mutable container : Dom_html.element Js.t }

let global_conf = { editor = None ;
		    container = Dom_html.document##body }

let editor () = match global_conf.editor with
    None -> failwith "Editor not init"
  | Some e -> e

let init_editor el =
  global_conf.editor <- Some (Ace.edit el);
  (editor())##setTheme(Js.string "ace/theme/eclipse")

let init_container el = global_conf.container <- el

