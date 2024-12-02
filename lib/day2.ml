(* Function to split line to integars *)
let parse_line : string -> int list =
 fun line ->
  line |> String.trim (* remove leading and trailing whitespaces *)
  |> String.split_on_char ' ' (* split at whitespace *)
  |> List.filter (fun s -> s <> "")
     (* remove empty substrings caused by empty spaces *)
  |> List.map int_of_string (* map list of strings and convert to integars *)

(* function to read a file and parse it into a list of integar lists *)
let parse_file : string -> int list list =
 fun filename ->
  let in_file : in_channel = open_in filename in
  let rec read_lines (acc : int list list) : int list list =
    try
      let line : string = input_line in_file in
      let report : int list = parse_line line in
      read_lines (report :: acc)
    with End_of_file ->
      close_in in_file;
      List.rev acc
  in
  read_lines []

let check_safe : int list -> bool =
 fun levels ->
  let rec check_difference : int list -> bool = function
    | [] | [ _ ] -> true
    | x :: y :: rest ->
        let diff : int = y - x in
        (abs diff >= 1 && abs diff <= 3) && check_difference (y :: rest)
  in

  let rec is_asc : int list -> bool = function
    | [] | [ _ ] -> true
    | x :: y :: rest -> x < y && is_asc (y :: rest)
  in

  let rec is_dsc : int list -> bool = function
    | [] | [ _ ] -> true
    | x :: y :: rest -> x > y && is_dsc (y :: rest)
  in

  (is_asc levels || is_dsc levels) && check_difference levels

let count_safe : int list list -> int =
 fun list ->
  List.fold_left
    (fun (acc : int) (report : int list) ->
      if check_safe report then acc + 1 else acc)
    0 list

let impl_dampner : int list -> bool =
 fun levels ->
  if check_safe levels then true
  else
    let rec remove_level (lst : int list) (idx : int) : bool =
      if idx >= List.length lst then false
      else
        let reduced = List.filteri (fun i _ -> i <> idx) lst in
        if check_safe reduced then true else remove_level lst (idx + 1)
    in
    remove_level levels 0

let count_dampend_safe : int list list -> int =
 fun list ->
  List.fold_left
    (fun (acc : int) (report : int list) ->
      if impl_dampner report then acc + 1 else acc)
    0 list

let part1 : string -> unit =
 fun filename ->
  let reports : int list list = parse_file filename in
  let result = count_safe reports in
  Printf.printf "%d\n" result

let part2: string -> unit =
  fun filename ->
    let reports: int list list = parse_file filename in
    let result = count_dampend_safe reports in
    Printf.printf "%d\n" result
