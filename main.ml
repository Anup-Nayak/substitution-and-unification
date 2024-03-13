open List;;

type symbol = (string * int);;
type signature = symbol list;;
type variable = string

type tree = V of variable | C of { node: symbol ; children: tree list };;

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

let rec wftree (sign : signature) (t : tree) : bool =
  let rec check_node ((s, arity) : symbol) (children : tree list) : bool =
    let rec check_children_validity (children : tree list) : bool =
      List.for_all (wftree sign) children
    in
    let expected_arity = List.length children in
    arity = expected_arity && check_children_validity children
  in
  match t with
  | V _ -> true 
  | C { node; children } -> check_node node children



let sig1 = [ ("0", 0); ("1", 0); ("+", 2); ("*", 2) ];;

let x = V "x";;
let y = V "y";;
let z = V "z";;

let zero = C {node = ("0", 0); children = []};;
let one = C {node = ("1", 0); children = []};;
let plus_zero_one = C {node = ("+", 2); children = [zero] };;
let times_one_x = C {node = ("*", 2); children = [one; x] };;
let plus_zero_y = C {node = ("+", 2); children = [zero; y] };;
let plus_timesonex_pluszeroy = C {node = ("+", 2) ; children = [times_one_x; plus_zero_y ] };;
let plus_timesonex_z = C {node = ("+", 2); children = [times_one_x; z ] };;



let rec ht t = match t with
    | V _ -> 0
    | C r ->
        let (s,n) = r.node
        in
        if n = 0 then
            0
        else 
            1+(fold_left max 0 (map ht r.children));;


let rec size t = match t with
    | V _ -> 1
    | C r -> 1+(fold_left (+) 0 (map size r.children) );;


(*print_endline(string_of_int(size plus_timesonex_pluszeroy));;*)



let rec vars (t : tree) : variable list =
  let rec collect_vars acc = function
    | V v -> v :: acc
    | C { node = _; children } ->
        List.fold_left (fun acc child -> collect_vars acc child) acc children
  in
  collect_vars [] t


(*
let vars_set = vars times_one_x


let () =
  print_endline "Variables in the tree:";
  List.iter (fun var -> print_endline var) vars_set
*)


let rec mirror (t : tree) : tree =
  match t with
  | V _ -> t  
  | C { node; children } ->
      let reversed_children = List.rev_map mirror children in
      C { node; children = reversed_children }

(*

let vars_set = vars (mirror plus_timesonex_z)


let () =
  print_endline "Variables in the tree:";
  List.iter (fun var -> print_endline var) vars_set

*)

type substitution = (variable * tree) list

let rec has_duplicates = function
  | [] -> false
  | x :: xs -> List.exists (fun y -> x = y) xs || has_duplicates xs

let valid_substitution (sub : substitution) : bool =
  not (has_duplicates (List.map fst sub))

let rec subst (t : tree) (sub : substitution) : tree =
  let apply_sub sub var =
    match List.assoc_opt var sub with
    | None -> V var  
    | Some tree -> tree
  in
  match t with
  | V var -> apply_sub sub var  
  | C { node; children } -> 
      let substituted_children = List.map (fun child -> subst child sub) children in
      C { node; children = substituted_children }


let rec compose_substitutions (sub1 : substitution) (sub2 : substitution) : substitution =
    match sub1 with 
    | [] -> sub2
    | (var,tree1) :: rest -> 
        (var, subst tree1 sub2) :: compose_substitutions rest sub2;;

(*
let tree1 = C { node = ("+",2); children = [V "y"; V "z"] }
let sub1 = [("x", tree1)]
let sub2 = [("y", V "a"); ("z", V "b")]

let tree2 = C { node = ("*", 1); children = [V "x"; C { node = ("-", 3); children = [V "w"; V "x"] }] }
let sub3 = [("x", V "c")]
let expected_result1 = C { node = ("*", 1); children = [V "c"; C { node = ("-", 3); children = [V "w"; V "c"] }] }

let tree3 = C { node = ("/", 4); children = [V "p"; V "q"] }
let sub4 = [("m", V "n")]
let expected_result2 = C { node = ("/", 4); children = [V "p"; V "q"] }

let tree5 = C { node = ("^", 2); children = [V "a"; V "b"] }
let sub7 = [("a", V "x")]
let expected_result5 = C { node = ("^", 2); children = [V "x"; V "b"] }

let composed_sub = compose_substitutions sub4 sub7;;
let substituted_tree = subst tree5 sub7;;

let () =
  print_endline "Composed Substitution:";
  List.iter (fun (var, _) -> print_endline var) composed_sub;;

assert (composed_sub = [("m", V "n");("a", V "x")]);;
assert (substituted_tree = expected_result5);;
*)

