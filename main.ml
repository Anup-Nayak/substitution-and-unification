type symbol = (string * int);;

type signature = symbol list;;

type tree = V of string | C of { node: symbol ; children: tree list };;

(*helper function1 that checks duplicates in signature*)
let rec check_dup_symbols symbols_seen = function
    | [] -> true
    | (sym, _) :: t ->
        if List.mem sym symbols_seen then
            false
        else if sym = "" then
            false
        else
            check_dup_symbols (sym :: symbols_seen) t;;


(*helper function2 that checks negative arities in signature*)
let rec check_non_negative_arity = function
    | [] -> true
    | (_, arity) :: t ->
        if arity < 0 then
            false
        else
            check_non_negative_arity t;;



let rec check_sig (sig_list : signature) : bool =
    check_dup_symbols [] sig_list && check_non_negative_arity sig_list;;


(*

let test0 = [("x", 0)];; (*true*)
let test1 = [("+",1);("1",0);("0",0)];; (*true*)
let test2 = [("+",1);("1",-1);("0",0)];; (*false*)
let test3 = [("+",1);("1",0);("0",0);("1",0)];; (*false*)
let test4 = [("a", 1); ("b", 0); ("a", 2)];; (*false*)
let test5 = [("", 0); ("y", 1); ("z", 2)];; (*false*)
let test6 = [("a", 0); ("a", 1); ("z", 2)];; (*false*)

let ans0 = check_sig test0;;
let ans1 = check_sig test1;;
let ans2 = check_sig test2;;
let ans3 = check_sig test3;;
let ans4 = check_sig test4;;
let ans5 = check_sig test5;;
let ans6 = check_sig test6;;

print_endline(string_of_bool(check_sig test0));;
print_endline(string_of_bool(check_sig test1));;
print_endline(string_of_bool(check_sig test2));;
print_endline(string_of_bool(check_sig test3));;
print_endline(string_of_bool(check_sig test4));;
print_endline(string_of_bool(check_sig test5));;
print_endline(string_of_bool(check_sig test6));;
*)




