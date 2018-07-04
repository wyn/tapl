open UntypedArith

let test = 
  let z = (TmZero dummyinfo) in
  let s = (TmSucc (dummyinfo, z)) in
  let ex1 = eval s in 
  let ex2 = eval_bigstep s in 
  let s1 =(string_of_bool (isnumerical ex1)) in
  let s2 = (string_of_bool (isnumerical ex2)) in
  print_string (s1 ^ ", " ^ s2)

let () =
  test ;
  print_string "\ndone\n"
             
