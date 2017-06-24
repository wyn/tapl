open UntypedArith
;;

let test = 
  let z = (TmZero dummyinfo) in
  let s = (TmSucc (dummyinfo, z)) in
  let ex = eval s in 
  print_string (string_of_bool (isnumerical ex))

let () =
  test ;
  print_string "\ndone\n"
             
