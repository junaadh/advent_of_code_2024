let read_as_grid (filename: string): string array =
  let in_file = open_in filename in
  let rec read_ (acc: string list): string list =
    try
      let line = input_line in_file in
      read_ (line :: acc)
    with End_of_file ->
      close_in in_file;
      List.rev acc
  in
  let lines = read_ [] in
  Array.of_list lines
    
let directions = [
    (0,1);
    (1,0);
    (0,-1);
    (-1,0);
    (1,1);
    (1,-1);
    (-1,1);
    (-1,-1);
  ]

let find_word (grid: string array) (word: string) ((row, col): (int * int)) ((row_i, col_i): (int * int)): bool =
  let len = String.length word in
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  (* Printf.printf "row: %d in rows: %d; col: %d in cols: %d; direction: (%d,%d)" row rows col cols row_i col_i; *)
  try
    for i = 0 to len - 1 do
      let r = row + (i * row_i) in
      let c = col + (i * col_i) in
      if r < 0 || r >= rows || c < 0 || c >= cols || grid.(r).[c] <> word.[i] then
        raise Exit
      (* else *)
        (* Printf.printf "%c;%c\n" word.[i] grid.(r).[c]; *)
    done;
    true
  with Exit ->
    false

let count (grid: string array) (word: string): int =
  let count = ref 0 in
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      List.iter (fun (dir: (int * int)) -> 
        if find_word grid word (row, col) dir then
          incr count
      ) directions
    done
  done;
  !count

let test = Array.of_list [
    "MMMSXXMASM";
    "MSAMXMSMSA";
    "AMXSXMAAMM";
    "MSAMASMSMX";
    "XMASAMXAMM";
    "XXAMMXXAMA";
    "SMSMSASXSS";
    "SAXAMASAAA";
    "MAMMMXMMMM";
    "MXMXAXMASX";
  ]

let part1 (filename: string): unit =
  let grid = read_as_grid filename in
  (* let grid = test in *)
  let word = "XMAS" in
  let occurence = count grid word in
  Printf.printf "%d\n" occurence

let is_xmas grid (row, col) =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  try
    (* Forward X-MAS *)
    if row - 1 >= 0 && row + 1 < rows &&
       col - 1 >= 0 && col + 1 < cols &&
       grid.(row - 1).[col - 1] = 'M' &&
       grid.(row - 1).[col + 1] = 'S' &&
       grid.(row).[col] = 'A' &&
       grid.(row + 1).[col - 1] = 'M' &&
       grid.(row + 1).[col + 1] = 'S' then
      true
    else if row - 1 >= 0 && row + 1 < rows &&
            col - 1 >= 0 && col + 1 < cols &&
            (* Backward X-MAS *)
            grid.(row - 1).[col + 1] = 'M' &&
            grid.(row - 1).[col - 1] = 'S' &&
            grid.(row).[col] = 'A' &&
            grid.(row + 1).[col + 1] = 'M' &&
            grid.(row + 1).[col - 1] = 'S' then
      true
    else if row - 1 >= 0 && row + 1 < rows &&
            col - 1 >= 0 && col + 1 < cols &&
            (* Backward X-MAS *)
            grid.(row - 1).[col + 1] = 'S' &&
            grid.(row - 1).[col - 1] = 'S' &&
            grid.(row).[col] = 'A' &&
            grid.(row + 1).[col + 1] = 'M' &&
            grid.(row + 1).[col - 1] = 'M' then
      true
    else if row - 1 >= 0 && row + 1 < rows &&
            col - 1 >= 0 && col + 1 < cols &&
            (* Backward X-MAS *)
            grid.(row - 1).[col + 1] = 'M' &&
            grid.(row - 1).[col - 1] = 'M' &&
            grid.(row).[col] = 'A' &&
            grid.(row + 1).[col + 1] = 'S' &&
            grid.(row + 1).[col - 1] = 'S' then
      true
    else false
  with _ -> false

(* Count all "X-MAS" occurrences in the grid *)
let count_xmas grid =
  let rows = Array.length grid in
  let cols = String.length grid.(0) in
  let count = ref 0 in
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      if is_xmas grid (row, col) then incr count
    done
  done;
  !count

let part2 (filename: string) : unit =
  let grid = read_as_grid filename in
  (* let grid = test in *)
  let occurences = count_xmas grid in
  Printf.printf "%d\n" occurences
