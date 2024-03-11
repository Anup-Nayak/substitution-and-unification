type symbol = string;;

type signature = (symbol * int) list;;

type tree = V of string | C of { node: symbol ; children: tree list };;

let rec check_dup_symbols symbols_seen = function
    | [] -> true
    | (sym, _) :: t ->
        if List.mem sym symbols_seen then
          false
        else
          check_dup_symbols (sym :: symbols_seen) t;;

let rec check_non_negative_arity = function
    | [] -> true
    | (_, arity) :: t ->
        if arity < 0 then
          false
        else
          check_non_negative_arity t

let rec check_sig (sig_list : signature) : bool =
  check_dup_symbols [] sig_list && check_non_negative_arity sig_list;;


let test1 = [("+",1);("1",0);("0",0);("1",0)];;

let ans = check_sig test1;;

print_endline(string_of_bool(check_sig test1))
