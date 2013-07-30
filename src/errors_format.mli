
type err_format = 
    { file : string;
      line : int;
      chars : int * int }

type errors = err_format list
