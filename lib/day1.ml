let read_cols filename =
  let rec read_lines in_file col1 col2 =
    try
      let line = input_line in_file in
      let numbers =
        line
        |> String.split_on_char ' '
        |> List.filter (fun s -> s <> "")
      in
      match numbers with
      | [num1; num2] ->
        read_lines in_file (int_of_string num1 :: col1) (int_of_string num2 :: col2)
      | _ -> failwith "Invalid line format"
    with
    | End_of_file -> (List.rev col1, List.rev col2)
  in
  try
    let in_file = open_in filename in
    let columns = read_lines in_file [] [] in
    close_in in_file;
    Ok columns
  with
  | Sys_error err -> Error err
  | Failure err -> Error err

let process_columns col1 col2 =
  let sorted1 = List.sort compare col1 in
  let sorted2 = List.sort compare col2 in

  let mapped = List.map2 (fun a b -> max a b - min a b) sorted1 sorted2 in
  List.fold_left ( + ) 0 mapped

let main1 filename =
  match read_cols filename with
  | Ok (col1, col2) ->
    let result = process_columns col1 col2 in
    Printf.printf "Result: %d\n" result 
  | Error err -> Printf.eprintf "Error: %s\n" err

let occurences x lst =
  List.fold_left (fun acc y -> if y = x then acc + 1 else acc) 0 lst

let process_columns2 col1 col2 =
  let sorted1 = List.sort compare col1 in
  List.fold_left
  (fun acc x ->
    let count = occurences x col2 in
    acc + (x * count)
  )
  0
  sorted1

let main2 filename =
  match read_cols filename with
  | Ok (col1, col2) ->
      let result = process_columns2 col1 col2 in
    Printf.printf "Result: %d\n" result
  | Error err -> Printf.eprintf "Error: %s\n" err
