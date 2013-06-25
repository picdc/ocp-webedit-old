
exception Bad_request_url of string

let pull_request ~callback ~meth ~url ~asyn ~msg =
(* let pull_request ~post_args ~url = *)
  (* let open XmlHttpRequest in *)
  (* let urlopt = Url.url_of_string url in *)
  (* match urlopt with  *)
  (* | None -> raise (Bad_request_url url) *)
  (* | Some url -> *)
  (*   let httpframe = Lwt_main.run (perform ~post_args url) in *)
  (*   httpframe.content *)
  let req = XmlHttpRequest.create () in
  req##_open(Js.string meth, Js.string url, Js.bool asyn);
  req##setRequestHeader(Js.string "Content-Type",
			Js.string "application/x-www-form-urlencoded");
  let f () = 
    match req##readyState with
    | XmlHttpRequest.DONE -> callback (Js.to_string req##responseText)
    | _ -> ()
  in
  req##onreadystatechange <- Js.wrap_callback f;
  req##send(Js.some (Js.string msg))



let get_list_of_projects ~callback =
  let callback str =
    (* Rappel :
       dans la réponse de la requête, la derniere ligne est vide *)
    let l = Ace_utils.split str "\n" in
    let l = List.rev (List.tl (List.rev l)) in
    callback l
  in
  pull_request ~callback ~meth:"POST" ~url:"project" ~asyn:true ~msg:""


let get_list_of_files ~callback project =
  let callback str =
    (* Rappel :
       dans la réponse de la requête, la derniere ligne est vide *)
    let l = Ace_utils.split str "\n" in
    let l = List.rev (List.tl (List.rev l)) in
    callback l
  in
  let msg = Format.sprintf "project=%s" project in
  pull_request ~callback ~meth:"POST" ~url:"project/list" ~asyn:true ~msg


let get_content_of_file ~callback ~project ~filename =
  let msg = Format.sprintf "project=%s&file=%s" project filename in
  pull_request ~callback ~meth:"POST" ~url:"project/load" ~asyn:true ~msg


let create_project ~callback name =
  let msg = Format.sprintf "name=%s" name in
  let callback _ = callback () in 
  pull_request ~callback ~meth:"POST" ~url:"create" ~asyn:true ~msg


let create_file ~callback ~project ~filename =
  let msg = Format.sprintf "project=%s&name=%s" project filename in
  let callback _ = callback () in 
  pull_request ~callback ~meth:"POST" ~url:"project/create" ~asyn:true ~msg


let save_file ~callback ~project ~filename ~content =
  let msg = Format.sprintf "project=%s&file=%s&content=%s"
    project filename (Url.urlencode content) in
  Ace_utils.console_log msg;
  let callback _ = callback () in
  pull_request ~callback ~meth:"POST" ~url:"project/save" ~asyn:true ~msg


let rename_file ~callback ~project ~filename ~new_name =
  let msg = Format.sprintf "project=%s&file=%s&newname=%s"
    project filename new_name in
  let callback _ = callback () in
  pull_request ~callback ~meth:"POST" ~url:"project/rename" ~asyn:true ~msg


let rename_project ~callback ~project ~new_name =
  let msg = Format.sprintf "project=%s&newname=%s" project new_name in
  let callback _ = callback () in
  pull_request ~callback ~meth:"POST" ~url:"rename" ~asyn:true ~msg


let delete_project ~callback ~project =
  let msg = Format.sprintf "project=%s" project in
  let callback _ = callback () in
  pull_request ~callback ~meth:"POST" ~url:"delete" ~asyn:true ~msg


let delete_file ~callback ~project ~filename =
  let msg = Format.sprintf "project=%s&file=%s" project filename in
  let callback _ = callback () in
  pull_request ~callback ~meth:"POST" ~url:"project/delete" ~asyn:true ~msg

