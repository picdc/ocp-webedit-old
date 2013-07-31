
open Errors_format

let make_error_report project error =
  (* let id = Filemanager.get_id ~project ~filename:error.file in *)
  Dom_html.handler (fun _ ->
    let id = Filemanager.get_id ~project ~filename:error.file in
    if Filemanager.is_file_opened ~project ~filename:error.file then
      begin
        Eventmanager.switch_file#trigger id;
        let e = Global.editor () in
        let r = Ace.range 
          (error.line -1) (fst error.chars)
          (error.line -1) (snd error.chars)
        in
        if fst error.chars >= 0 then
          e##getSelection()##setSelectionRange(r, Js._false)
        else (e##moveCursorTo((error.line - 1), 0);
              e##getSelection()##selectLine())
      end
    else
      Eventmanager.go_to_next_error#trigger (project, error);
    Js._true)
    
let add_errors_reports project errors =
  ignore
    (List.fold_left 
       (fun i e ->
         let a = Myutils.get_element_by_id ("error" ^ (string_of_int i)) in
         a##onclick <- make_error_report project e;
         i+1)
       0
       errors)
