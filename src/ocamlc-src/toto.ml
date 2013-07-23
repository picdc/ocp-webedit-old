let _ =
  let s = "Pouet" in
  let s2 = "Pouet" in
  let out = open_out "test.txt" in 
  let a = s,s2 in
  output_value stdout a
   
