open Printf
open Str

let read_file (file: string): string =
  let file_ = open_in file in
  let rec read_(acc: string) =
  try
    let line = input_line file_ in
    read_ (acc ^ line)
  with End_of_file ->
    close_in file_;
    acc
  in
  read_ ""

let rec all_parts (reg: regexp) (input: string) (pos: int) (acc: string list): string list =
  try
    let _ = search_forward reg input pos in
    all_parts reg input (match_end ()) (matched_string input :: acc)
  with Not_found -> acc

let parse_mul (input: string): int =
  let nums = all_parts (regexp {|[0-9]+|}) input 0 [] in
  nums |> List.map int_of_string |> List.fold_left ( * ) 1

let reg: regexp = regexp {|mul([0-9]+,[0-9]+)|}


(* Main program *)
let part1 (filename: string): unit =
  let memory_string = read_file filename in  
  let all_matches = all_parts reg memory_string 0 [] in

  let total_result = List.map parse_mul all_matches |> List.fold_left ( + ) 0 in
  printf "%d\n" total_result

let part2 (filename: string): unit =
  let memory_string = read_file filename in
  let do_s = split (regexp "don't()") memory_string in
  let tl_do_s =
    List.tl do_s |> List.map (split (regexp "do()")) |> List.(map tl)
  in
  let all_do_s = List.hd do_s :: List.flatten tl_do_s |> String.concat "" in

  let all_matches = all_parts reg all_do_s 0 [] in
  let total_results = List.map parse_mul all_matches |> List.fold_left ( + ) 0 in
  printf "%d\n" total_results
