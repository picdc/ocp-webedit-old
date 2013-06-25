(**
   Contains all the words for autocompletion
   Could be more complex in the future
**)

module Words = Set.Make(String)

let words = ref Words.empty

let origin = ref ""

let completions = ref (Array.make 0 "")
let actual_index = ref 0

let new_word w =
  words :=  Words.add w !words

