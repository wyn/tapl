type info =
  Line of int
| Dummy
;;


type term =
  TmTrue of info
| TmFalse of info
| TmIf of info * term * term * term
| TmZero of info
| TmSucc of info * term
| TmPred of info * term
| TmIsZero of info * term
;;
  
let rec isnumerical t = match t with
  TmZero _ -> true
| TmSucc (_, rest) -> isnumerical rest
| _ -> false

  
let rec isval t = match t with
  TmTrue _ -> true
| TmFalse _ -> true
| t when (isnumerical t) -> true
| _ -> false

  
exception NoRuleApplies
;;
  
let dummyinfo = Dummy
;;

let rec eval1 t = match t with
  TmIf (_, TmTrue (_), t2, t3) -> t2
| TmIf (_, TmFalse (_), t2, t3) -> t3
| TmIf (fi, t1, t2, t3) ->
    let t1' = eval1 t1 in
    TmIf (fi, t1', t2, t3)
| TmSucc (fi, t1) ->
    let t1' = eval1 t1 in
    TmSucc (fi, t1')
| TmPred (_, TmZero (_)) -> TmZero dummyinfo
| TmPred (_, TmSucc ((_, nv))) when (isnumerical nv) -> nv
| TmPred (fi, t1) ->
    let t1' = eval1 t1 in
    TmPred (fi, t1')
| TmIsZero (_, (TmZero _)) ->
    TmTrue dummyinfo
| TmIsZero (_, (TmSucc (_, nv))) when (isnumerical nv) ->
    TmFalse dummyinfo
| TmIsZero (fi, t1) ->
  let t1' = eval1 t1 in
  TmIsZero (fi, t1')
| _ -> raise NoRuleApplies


let rec eval t =
  try let t' = eval1 t
      in eval t'
  with NoRuleApplies -> t
;;

(* TODO big step style *)
let rec eval2 t = match t with
  TmIf (_, TmTrue (_), t2, t3) -> eval2 t2
| TmIf (_, TmFalse (_), t2, t3) -> eval2 t3
| TmIf (fi, t1, t2, t3) ->
    let t1' = eval2 t1 in
    TmIf (fi, t1', t2, t3)
| TmSucc (fi, t1) ->
    let t1' = eval2 t1 in
    TmSucc (fi, t1')
| TmPred (_, TmZero (_)) -> TmZero dummyinfo
| TmPred (_, TmSucc ((_, nv))) when (isnumerical nv) -> eval2 nv
| TmPred (fi, t1) ->
    let t1' = eval2 t1 in
    TmPred (fi, t1')
| TmIsZero (_, (TmZero _)) ->
    TmTrue dummyinfo
| TmIsZero (_, (TmSucc (_, nv))) when (isnumerical nv) ->
    TmFalse dummyinfo
| TmIsZero (fi, t1) ->
  let t1' = eval2 t1 in
  TmIsZero (fi, t1')
| _ -> raise NoRuleApplies


let rec eval_bigstep t =
  try let t' = eval2 t
      in eval_bigstep t'
  with NoRuleApplies -> t
;;
