(* 5の倍数かどうかを判断するオートマトン *)

(* アルファベットΣ={0, 1} *)
type alphabet = | Zero | One;;
let alphabet_num = 2;;

(* 状態は名前と受理状態かどうか *)
type state = S of string * bool * string list;;

(* 使用する状態は5で割った時のあまり
 * 開始状態はs、受理状態はq0だけ *)
let states : state list =
  [
    S ("s", false, ["q0"; "q1"]); (* 初期状態 *)
    S ("q0", true, ["q0"; "q1"]); (* 受理状態 *)
    S ("q1", false, ["q2"; "q3"]);
    S ("q2", false, ["q4"; "q0"]);
    S ("q3", false, ["q1"; "q2"]);
    S ("q4", false, ["q3"; "q4"]);
  ]
;;

(* strという名前のstateをstatesから見つける *)
let get_state (sts: state list) (str:string): state =
  let rec get_state idx =
    let s = List.nth sts idx in
    match s with S(sname, _, _) ->
      if sname = str then List.nth sts idx
      else get_state @@ idx+1
  in
  get_state 0
;;

(* stsのstateの遷移に則り、state sにalphabet aが入力された時の遷移先 *)
let transition_state (sts: state list) (s:state) (a:alphabet): state =
  let alphabet_to_index a = match a with |Zero -> 0 |One -> 1 in
  match s with S(_, _, lst) -> get_state sts (List.nth lst @@ alphabet_to_index a)
;;

let s0 = get_state states "q0";;
let s1 = transition_state states s0 Zero;;
let s2 = transition_state states s0 One;;

let w1 = "101" (* 5 *)
let w2 = "1011" (* 11 a*)
let w3 = "11" (* 3 *)

(* chrをalphabetに変関する *)
let chr_to_alphabet c = match c with
  | '0' -> Zero
  | '1' -> One
  | _ -> failwith "Invalid alphabet"
;;

(* stringをalphabetのリストにする *)
let w_to_list (w:string) : alphabet list =
  let rec w_to_list i (k : alphabet list -> alphabet list) =
    if i = String.length w then k []
    else w_to_list (i+1) (fun res -> k ((chr_to_alphabet @@ String.get w i) :: res))
  in
  w_to_list 0 (fun x -> x)
;;

let lst1 = w_to_list w1;;
let lst2 = w_to_list w2;;
let lst3 = w_to_list w3;;

(* 開始状態からalphabet listを入力したら受理状態になるかどうか *)
let judge_accept (sts: state list) (lst:alphabet list) : bool =
  let s0 = get_state sts "s" in
  let rec transition (s: state) (lst: alphabet list) =
    match lst with
    | [] -> s
    | h::t -> transition (transition_state sts s h) t
  in
  let e = (transition s0) lst in
  match e with
  | S (_, b, _) -> b
;;

(* 整数を2進数のstringに変換 *)
let int_to_binary (n:int) : string =
  let rec int_to_binary (n:int) (res:string): string =
    if n = 0 then res
    else int_to_binary (n/2) @@ (string_of_int @@ n mod 2)^res
  in
  if n = 0 then "0"
  else int_to_binary n ""
;;

(* n mod5 が0かどうかを判定*)
let check_mod_5 (n:string) : bool = judge_accept states @@ w_to_list n;;

(* 期待通りの挙動をするかどうか、順番に試してみる *)
let test_check_function n lim : bool =
  let rec test_check_function n lim res : bool =
    if n > lim then res
    else
      begin
        let w = int_to_binary n in
        let b1 = (n mod 5 = 0) in
        let b2 = check_mod_5 w in
        let r = (b1=b2) in
        print_string ((string_of_int n)^" -> "^w^" -> "^(if b1 then "t" else "f")^" -> "^(if b2 then "t" else "f")^" -> "^(if r then "OK" else "NG")^"\n");
        test_check_function (n+1) lim (res && r)
      end
  in
  test_check_function n lim true
;;


(* 10000まで試して見たらとりあえずできてるっぽい *)
test_check_function 0 10000;;

