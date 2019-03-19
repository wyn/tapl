
let test =
  let module UA = Tapl.UntypedArith in 
  let z = (UA.TmZero UA.dummyinfo) in
  let s = (UA.TmSucc (UA.dummyinfo, z)) in
  let ex1 = UA.eval s in 
  let ex2 = UA.eval_bigstep s in 
  let s1 =(string_of_bool (UA.isnumerical ex1)) in
  let s2 = (string_of_bool (UA.isnumerical ex2)) in
  print_string (s1 ^ ", " ^ s2)

let () =
  test ;
  print_string "\ndone\n"
             
