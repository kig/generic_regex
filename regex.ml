type 'a range_atom =
    RChar of 'a
  | RRange of 'a * 'a
and 'a range = 'a range_atom list

type 'a regex_atom =
    Sub of 'a regex
  | Or of 'a regex * 'a regex
  | Char of 'a
  | Opt of 'a regex_atom
  | Star of 'a regex_atom
  | Plus of 'a regex_atom
  | Range of 'a range
  | Any
  | Start
  | End
and 'a regex = 'a regex_atom list

type 'a nfa =
    Node of 'a * 'a nfa
  | RangeNode of 'a range * 'a nfa
  | AnyNode of 'a nfa
  | StartNode of 'a nfa
  | EndNode of 'a nfa
  | Split of 'a nfa * 'a nfa
  | LoopSplit of 'a nfa * 'a nfa
  | Finish

exception No_closing_paren of int
exception No_opening_paren of int
exception No_closing_range of int
exception No_preceding_expression of int
exception Unknown_character of int
exception Unknown_range_character of int
exception Bad_range of int





let rec nfa_append a b = match a with
  | Finish -> b
  | Node (v,t) -> Node (v, (nfa_append t b))
  | RangeNode (v,t) -> RangeNode (v, (nfa_append t b))
  | AnyNode t -> AnyNode (nfa_append t b)
  | StartNode t -> StartNode (nfa_append t b)
  | EndNode t -> EndNode (nfa_append t b)
  | Split (t,u) -> Split (nfa_append t b, nfa_append u b)
  | LoopSplit (t,u) -> LoopSplit (t, nfa_append u b)

let rec nfa_of_regex r =
  match r with
    | [] -> Finish
    | Char c :: t -> Node (c, nfa_of_regex t)
    | Or (a,b) :: t ->
      let tnfa = nfa_of_regex t in
      Split (nfa_append (nfa_of_regex a) tnfa, nfa_append (nfa_of_regex b) tnfa)
    | Sub a :: t -> nfa_append (nfa_of_regex a) (nfa_of_regex t)
    | Opt a :: t -> 
      let tnfa = nfa_of_regex t in
      Split (nfa_append (nfa_of_regex [a]) tnfa, tnfa)
    | Star a :: t -> LoopSplit (nfa_of_regex [a], nfa_of_regex t)
    | Plus a :: t -> LoopSplit (nfa_of_regex [a], nfa_of_regex (a::t))
    | Range a :: t -> RangeNode (a, nfa_of_regex t)
    | Any :: t -> AnyNode (nfa_of_regex t)
    | Start :: t -> StartNode (nfa_of_regex t)
    | End :: t -> EndNode (nfa_of_regex t)

let range_match r c =
  List.exists (function
    | RChar a -> a = c
    | RRange (a,b) -> a <= c && c <= b
  ) r

let nfa_match state c =
  match state with
    | Node (a,_) -> a = c
    | RangeNode (r,_) -> range_match r c
    | AnyNode _ | Finish -> true
    | StartNode _ | EndNode _ -> false
    | _ -> failwith "nfa_match: tried to match Split or LoopSplit, not supported"

let advance_state state =
  match state with
    | Node (_,t) | RangeNode (_,t) | AnyNode t | StartNode t | EndNode t -> t
    | Finish -> Finish
    | _ -> failwith "advance_state: can't advance Split or LoopSplit"


let uniq l =
  let h = Hashtbl.create 16 in
  List.filter (fun (j,i) ->
    if Hashtbl.mem h i then false
    else (Hashtbl.replace h i true; true)
  ) l

let rec expand_states states =
  let s = List.fold_left (fun l (j,i) ->
    match i with
      | LoopSplit (a,b) ->
        (expand_states [(j,nfa_append a i)] @ expand_states [(j,b)] @ l)
      | Split (a,b) ->
        (expand_states [(j,a)] @ expand_states [(j,b)] @ l)
      | _ -> (j,i)::l
  ) [] states in
  uniq s

(* FIXME Implement greedy patterns
  "fo+" ~= "fooooo" should match the whole string, not just the beginning "fo"
*)
let execute_nfa nfa getter =
  let is_finish (_,i) = match i with Finish -> true | _ -> false in
  let is_start (_,i) = match i with StartNode _ -> true | _ -> false in
  let is_end (_,i) = match i with EndNode _ -> true | _ -> false in
  let rec aux start states getter i =
    let states = if i = 0
      then List.map (fun (i,s) -> i, advance_state s) (List.filter is_start (expand_states [i,start]))
      else states in
    let states = expand_states ((i, start)::states) in
    if List.exists is_finish states
    then Some (fst (List.find is_finish states), i)
    else
      match getter i with
        | Some c ->
          let states = List.filter (fun (_,s) -> nfa_match s c) states in
          let states = List.map (fun (i,s) -> i, advance_state s) states in
          aux start states getter (i+1)
        | None ->
          let states = List.map (fun (i,s) -> i, advance_state s) (List.filter is_end states) in
          if List.exists is_finish states
          then Some (fst (List.find is_finish states), i)
          else None in
  aux nfa [] getter 0






let rec parse_range res r i =
  if i >= String.length r
  then raise (No_closing_range i)
  else match r.[i] with
    | ']' -> (List.rev res, i)
    | '-' ->
      if r.[i+1] = ']'
      then parse_range (RChar '-' :: res) r (i+1)
      else (match res with
              | [] | RRange _ :: _ -> parse_range (RChar '-' :: res) r (i+1)
              | RChar h :: t ->
                let i = if r.[i+1] == '\\' then i + 1 else i in
                parse_range (RRange (h, r.[i+1]) :: t) r (i+2))
    | '\\' -> parse_range (RChar r.[i+1] :: res) r (i+2)
    | c -> parse_range (RChar c :: res) r (i+1)

let rec parse_regex_sub res r i =
  if i >= String.length r
  then raise (No_closing_paren i)
  else match r.[i] with
    | ')' -> (List.rev res, i)
    | '(' ->
      let sub, i = parse_regex_sub [] r (i+1) in
      parse_regex_sub (Sub sub :: res) r (i+1)
    | '[' ->
      let sub, i = parse_range [] r (i+1) in
      parse_regex_sub (Range sub :: res) r (i+1)
    | '|' ->
      let sub, i = parse_regex_sub [] r (i+1) in
      ([Or (List.rev res, sub)], i)
    | '?' -> (match res with
      | [] -> raise (No_preceding_expression i)
      | h::t -> parse_regex_sub (Opt h :: t) r (i+1))
    | '*' -> (match res with
      | [] -> raise (No_preceding_expression i)
      | h::t -> parse_regex_sub (Star h :: t) r (i+1))
    | '+' -> (match res with
      | [] -> raise (No_preceding_expression i)
      | h::t -> parse_regex_sub (Plus h :: t) r (i+1))
    | '.' -> parse_regex_sub (Any :: res) r (i+1)
    | '^' -> parse_regex_sub (Start :: res) r (i+1)
    | '$' -> parse_regex_sub (End :: res) r (i+1)
    | '\\' -> parse_regex_sub (Char r.[i+1] :: res) r (i+2)
    | c -> parse_regex_sub (Char c :: res) r (i+1)

let parse_regex r =
  let sub, i = parse_regex_sub [] (r^")") 0 in
  if i < String.length r
  then raise (No_opening_paren (i-1))
  else sub






let int_of_substring s i =
  let rec aux s i =
    if i >= String.length s then (String.length s - 1)
    else match s.[i] with
      | '-' | '0'..'9' | 'a'..'f' | 'A'..'F' | 'x' | 'o' -> aux s (i+1)
      | _ -> (i-1) in
  let e = aux s i in
  let f = int_of_string (String.sub s i (e-i+1)) in
  let e = if e <> String.length s - 1 && s.[e+1] = ';' then e+2 else e+1 in
  (f,e)

let rec int_range res r i =
  if i >= String.length r
  then raise (No_closing_range i)
  else match r.[i] with
    | ']' -> (List.rev res, i)
    | '.' ->
      if r.[i+1] <> '.' then raise (Unknown_range_character (i+1));
      (match res with
        | [] | RRange _ :: _ -> raise (Bad_range i)
        | RChar h :: t ->
          let v, i = int_of_substring r (i+2) in
          int_range (RRange (h, v) :: t) r i)
    | '-' | '0'..'9' ->
      let v, i = int_of_substring r i in
      int_range (RChar v :: res) r i
    | ';' | ' ' -> int_range res r (i+1)
    | _ -> raise (Unknown_range_character i)

let rec int_regex_sub res r i =
  if i >= String.length r
  then raise (No_closing_paren i)
  else match r.[i] with
    | ')' -> (List.rev res, i)
    | '(' ->
      let sub, i = int_regex_sub [] r (i+1) in
      int_regex_sub (Sub sub :: res) r (i+1)
    | '[' ->
      let sub, i = int_range [] r (i+1) in
      int_regex_sub (Range sub :: res) r (i+1)
    | '|' ->
      let sub, i = int_regex_sub [] r (i+1) in
      ([Or (List.rev res, sub)], i)
    | '?' -> (match res with
      | [] -> raise (No_preceding_expression i)
      | h::t -> int_regex_sub (Opt h :: t) r (i+1))
    | '*' -> (match res with
      | [] -> raise (No_preceding_expression i)
      | h::t -> int_regex_sub (Star h :: t) r (i+1))
    | '+' -> (match res with
      | [] -> raise (No_preceding_expression i)
      | h::t -> int_regex_sub (Plus h :: t) r (i+1))
    | '_' -> int_regex_sub (Any :: res) r (i+1)
    | '^' -> int_regex_sub (Start :: res) r (i+1)
    | '$' -> int_regex_sub (End :: res) r (i+1)
    | '0'..'9' | '-' ->
      let v, i = int_of_substring r i in
      int_regex_sub (Char v :: res) r i
    | ';' | ' ' -> int_regex_sub res r (i+1)
    | c -> raise (Unknown_character i)

let int_regex r =
  let sub, i = int_regex_sub [] (r^")") 0 in
  if i < String.length r
  then raise (No_opening_paren (i-1))
  else sub






let float_of_substring s i =
  let exponent s i =
    let rec aux s i =
      if i >= String.length s then (String.length s - 1)
      else match s.[i] with
        | '0'..'9' -> aux s (i+1)
        | _ -> i-1 in
    aux s (match s.[i] with '-' | '+' -> i+1 | _ -> i) in
  let rec decimal s i = 
    if i >= String.length s then (String.length s - 1)
    else match s.[i] with
      | '0'..'9' -> decimal s (i+1)
      | 'e' | 'E' -> exponent s (i+1)
      | _ -> i-1 in
  let rec aux s i =
    if i >= String.length s then (String.length s - 1)
    else match s.[i] with
      | '0'..'9' -> aux s (i+1)
      | '.' -> if s.[i+1] = '.' then i-1 else decimal s (i+1)
      | 'e' | 'E' -> exponent s (i+1)
      | _ -> i-1 in
  let aux' s i = aux s (match s.[i] with '-' | '+' -> i+1 | _ -> i) in
  let e = aux' s i in
  let f = float_of_string (String.sub s i (e-i+1)) in
  let e = if e <> String.length s - 1 && s.[e+1] = ';' then e+2 else e+1 in
  (f,e)

let rec float_range res r i =
  if i >= String.length r
  then raise (No_closing_range i)
  else match r.[i] with
    | ']' -> (List.rev res, i)
    | '.' ->
      if r.[i+1] <> '.' then raise (Unknown_range_character (i+1));
      (match res with
        | [] | RRange _ :: _ -> raise (Bad_range i)
        | RChar h :: t ->
          let v, i = float_of_substring r (i+2) in
          float_range (RRange (h, v) :: t) r i)
    | '-' | '0'..'9' ->
      let v, i = float_of_substring r i in
      float_range (RChar v :: res) r i
    | ';' | ' ' -> float_range res r (i+1)
    | _ -> raise (Unknown_range_character i)

let rec float_regex_sub res r i =
  if i >= String.length r
  then raise (No_closing_paren i)
  else match r.[i] with
    | ')' -> (List.rev res, i)
    | '(' ->
      let sub, i = float_regex_sub [] r (i+1) in
      float_regex_sub (Sub sub :: res) r (i+1)
    | '[' ->
      let sub, i = float_range [] r (i+1) in
      float_regex_sub (Range sub :: res) r (i+1)
    | '|' ->
      let sub, i = float_regex_sub [] r (i+1) in
      ([Or (List.rev res, sub)], i)
    | '?' -> (match res with
      | [] -> raise (No_preceding_expression i)
      | h::t -> float_regex_sub (Opt h :: t) r (i+1))
    | '*' -> (match res with
      | [] -> raise (No_preceding_expression i)
      | h::t -> float_regex_sub (Star h :: t) r (i+1))
    | '+' -> (match res with
      | [] -> raise (No_preceding_expression i)
      | h::t -> float_regex_sub (Plus h :: t) r (i+1))
    | '_' -> float_regex_sub (Any :: res) r (i+1)
    | '^' -> float_regex_sub (Start :: res) r (i+1)
    | '$' -> float_regex_sub (End :: res) r (i+1)
    | '0'..'9' | '-' ->
      let v, i = float_of_substring r i in
      float_regex_sub (Char v :: res) r i
    | ';' | ' ' -> float_regex_sub res r (i+1)
    | c -> raise (Unknown_character i)

let float_regex r =
  let sub, i = float_regex_sub [] (r^")") 0 in
  if i < String.length r
  then raise (No_opening_paren (i-1))
  else sub






let string_of_range r =
  let esc buf c =
    (match c with | ']' | '-' -> Buffer.add_char buf '\\' | _ -> ());
    Buffer.add_char buf c in
  let rec aux buf = function
    | [] -> ()
    | h::t ->
      begin match h with
        | RRange (a,b) ->
          esc buf a;
          Buffer.add_char buf '-';
          esc buf b
        | RChar a ->
          esc buf a
      end;
      aux buf t in
  let buf = Buffer.create 16 in
  aux buf r;
  Buffer.contents buf

let special_char = function
  | '|' | '(' | ')' | '?' | '*' | '+' | '.' | '[' | '^' | '$' | '\\' -> true
  | c -> false

let unescape s =
  let rec aux buf s l i =
    if i > l then failwith "unescape"
    else if i = l then Buffer.contents buf
    else begin
      let i = if s.[i] = '\\' then i + 1 else i in
      if i = l then failwith "unescape: trailing backslash";
      Buffer.add_char buf s.[i];
      aux buf s l (i+1)
    end in
  let l = String.length s in
  aux (Buffer.create l) s l 0

let escape s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    if special_char c then Buffer.add_char buf '\\';
    Buffer.add_char buf c) s;
  Buffer.contents buf

let string_of_regex r =
  let rec aux buf = function
      | [] -> ()
      | h :: t ->
        begin match h with
          | Or (a,b) ->
            aux buf a;
            Buffer.add_char buf '|';
            aux buf b
          | Sub s ->
            Buffer.add_char buf '(';
            aux buf s;
            Buffer.add_char buf ')'
          | Opt s ->
            aux buf [s];
            Buffer.add_char buf '?'
          | Star s ->
            aux buf [s];
            Buffer.add_char buf '*'
          | Plus s ->
            aux buf [s];
            Buffer.add_char buf '+'
          | Any -> Buffer.add_char buf '.'
          | Start -> Buffer.add_char buf '^'
          | End -> Buffer.add_char buf '$'
          | Range r ->
            Buffer.add_char buf '[';
            Buffer.add_string buf (string_of_range r);
            Buffer.add_char buf ']'
          | Char c ->
            if special_char c then Buffer.add_char buf '\\';
            Buffer.add_char buf c
        end;
        aux buf t in
  let buf = Buffer.create 16 in
  aux buf r;
  Buffer.contents buf





let string_getter s =
  let l = String.length s in
  fun i -> if i >= l then None else Some s.[i]

let pat_match pat s =
  execute_nfa (nfa_of_regex (parse_regex pat)) (string_getter s)



let array_getter a =
  let l = Array.length a in
  fun i -> if i >= l then None else Some a.(i)

let int_match pat a =
  execute_nfa (nfa_of_regex (int_regex pat)) (array_getter a)

let float_match pat a =
  execute_nfa (nfa_of_regex (float_regex pat)) (array_getter a)




let sinit f len =
  let s = String.create len in
  for i = 0 to len-1 do s.[i] <- f i done;
  s

let assert_bool b = if b then () else failwith "assert_bool"

let assert_equal a b = assert_bool (a = b)

let () =
  let s = sinit char_of_int 256 in
  let ss = [
    "foo|bar(baz|qux)";
    "foo||||bar(a|b(cd|ef|gh)|i)(|or)";
    "he*l+p?";
    "foo\\|?";
    ".*|foo\000\001\255";
    "^foo|bar$";
    "bo[boa-z0-9]";
    "bo[\\-b\\-oa-z0-\\]9\\-]";
    escape s
  ] in
  List.iter (fun s -> assert_equal s (string_of_regex (parse_regex s))) ss;
  List.iter (fun s -> assert_equal s (unescape (escape s))) ss;
  assert_equal s (unescape (escape s))

let () =
  let ss = [
    "28;49;90;?[48..200;202..48;-49..-10]*-20";
    "^(30*20?)[10..20][4]11+2|5|7$";
    "10;_;20$";
    "0xff;0o47;0b1001"
  ] in
  List.iter (fun s -> ignore (int_regex s)) ss

let () =
  let ss = [
    "28;49;90;?[48..200;202..48;-49..-10]*-20";
    "^(30*20?)[10..20][4]11+2|5|7$";
    "10;_;20$";
    "^(3.02e2*20?)[10e-2..-20E+3][4e4]0.11+2|5|7$"
  ] in
  List.iter (fun s -> ignore (float_regex s)) ss
  
