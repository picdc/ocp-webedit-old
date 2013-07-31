
open Errors_format

let make_error_report project error =
  (* let id = Filemanager.get_id ~project ~filename:error.file in *)
  Dom_html.handler (fun _ ->
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
