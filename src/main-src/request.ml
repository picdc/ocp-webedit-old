
exception Bad_request_url of string

let pull_request ~callback ~meth ~url ~asyn ~msg =
  let req = XmlHttpRequest.create () in
  let url = "api/" ^ url in 
  req##_open(Js.string meth, Js.string url, Js.bool asyn);
  req##setRequestHeader(Js.string "Content-Type",
			Js.string "application/x-www-form-urlencoded");
  req##setRequestHeader(Js.string "withCredentials", Js.string "true");
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
    let l = Myparser.split str '\n' in
    let l = List.rev (List.tl (List.rev l)) in
    callback l
  in
  pull_request ~callback ~meth:"POST" ~url:"/project" ~asyn:true ~msg:""


let get_list_of_files ~callback project =
  let callback str =
    (* Rappel :
       dans la réponse de la requête, la derniere ligne est vide *)
    let l = Myparser.split str '\n' in
    let l = List.rev (List.tl (List.rev l)) in
    callback l
  in
  let msg = Format.sprintf "project=%s" project in
  pull_request ~callback ~meth:"POST" ~url:"/project/list" ~asyn:true ~msg


let get_content_of_file ~callback ~project ~filename =
  let msg = Format.sprintf "project=%s&file=%s" project filename in
  pull_request ~callback ~meth:"POST" ~url:"/project/load" ~asyn:true ~msg


let create_project ~callback name =
  let msg = Format.sprintf "name=%s" name in
  let callback _ = callback () in 
  pull_request ~callback ~meth:"POST" ~url:"/create" ~asyn:true ~msg


let create_file ~callback ~project ~filename =
  let msg = Format.sprintf "project=%s&name=%s" project filename in
  let callback _ = callback () in 
  pull_request ~callback ~meth:"POST" ~url:"/project/create" ~asyn:true ~msg


let save_file ~callback ~project ~filename ~content =
  let msg = Format.sprintf "project=%s&file=%s&content=%s"
    project filename (Url.urlencode content) in
  let callback _ = callback () in
  pull_request ~callback ~meth:"POST" ~url:"project/save" ~asyn:true ~msg

let import_file ~callback ~project ~filename ~content =
  let msg = Format.sprintf "project=%s&file=%s&content=%s"
    project filename (Url.urlencode content) in
  let callback _ = callback () in
  pull_request ~callback ~meth:"POST" ~url:"project/import" ~asyn:true ~msg

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

let export_project ~callback ~project =
  let msg = Format.sprintf "project=%s" project in
  let callback _ = callback () in
  pull_request ~callback ~meth:"POST" ~url:"export" ~asyn:true ~msg

let save_conf ~callback ~name ?(project=None) ~content =
  let msg = match project with
    | None -> Format.sprintf "file=%s&content=%s"
        name (Url.urlencode content)
    | Some s -> Format.sprintf "project=%s&file=%s&content=%s"
        s name (Url.urlencode content)
  in
  let callback _ = callback () in
  pull_request ~callback ~meth:"POST" ~url:"conf/save" ~asyn:true ~msg


let load_conf ~callback ~name ?(project=None) () =
  let msg = match project with
    | None -> Format.sprintf "file=%s" name
    | Some s -> Format.sprintf "project=%s&file=%s" s name
  in
  pull_request ~callback ~meth:"POST" ~url:"conf/load" ~asyn:true ~msg
