exception End_of_file

let print_diff orig comp out1 out2=
  let read_byte inp =
    let buff = String.create 4 in
    let i = input inp buff 0 4 in
    if i = 0 then raise End_of_file; 
    buff
  in

  (* Reads a header (if there is one) and return the first byte after *)
  let read_header inp =
    let rec step inp =
      let i = read_byte inp in
      if i = "\\010" then 
        read_byte inp
      else
        step inp
    in
    let i = read_byte inp in
    if i = "\\035" then
      step inp
    else
      i
  in
  
  try
    let fst = ref true in
    while true do
      let s1 = if !fst then read_header orig else read_byte orig in
      let s2 = if !fst then 
          (fst := false;
           read_header comp)                  
        else read_byte comp in
      if s1 <> s2 then
        (output_string out1 "\n";
         output_string out1 s1;
         output_string out1 "\n";
         output_string out2 "\n";
         output_string out2 s2;
         output_string out2 "\n")
      else
        (output_string out1 s1;
         output_string out2 s2)
    done
  with _ -> 
    close_out out1;
    close_out out2;
    close_in orig;
    close_in comp

let _ = 
  let orig = Sys.argv.(1) in
  let comp = Sys.argv.(2) in
  let out1 = open_out (orig^"_diff") in
  let out2 = open_out (comp^"_diff") in
  let orig = open_in orig in
  let comp = open_in comp in
  print_diff orig comp out1 out2
