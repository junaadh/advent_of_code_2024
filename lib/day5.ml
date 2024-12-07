let test : string =
  "47|53\n\
   97|13\n\
   97|61\n\
   97|47\n\
   75|29\n\
   61|13\n\
   75|53\n\
   29|13\n\
   97|29\n\
   53|29\n\
   61|53\n\
   97|53\n\
   61|29\n\
   47|13\n\
   75|47\n\
   97|75\n\
   47|61\n\
   75|61\n\
   47|29\n\
   75|13\n\
   53|13\n\n\
   75,47,61,53,29\n\
   97,61,53,29,13\n\
   75,29,13\n\
   75,97,47,61,53\n\
   61,13,29\n\
   97,13,75,29,47"

let parse_input (filename: string): (((int * int) list) * int list list) =
  let chanel = open_in filename in
  let rec read_rules (acc: (int * int) list): (int * int) list =
    match input_line chanel with
    | "" -> List.rev acc
    | line ->
      let line = String.trim line in
      if line == "" then List.rev acc
      else
        let rule =
          match String.split_on_char '|' line |> List.map int_of_string with
          | [x;y] -> (x,y)
          | _ -> failwith "Invalid rule"
        in
        read_rules (rule :: acc)
    | exception End_of_file -> failwith "Unexpected Eof in rules section"
  in
  let rec read_updates (acc: int list list): int list list =
    match input_line chanel with
    | line ->
      let line = String.trim line in
      if line == "" then read_updates acc
      else
        let update = 
          line
          |> String.split_on_char ','
          |> List.map String.trim 
          |> List.map int_of_string 
        in
        read_updates (update :: acc)
    | exception End_of_file -> List.rev acc
  in
  let rules = read_rules [] in
  let updates = read_updates [] in
  close_in chanel;
  (rules, updates)


let build_graph (rules: (int * int) list) : (int, int list) Hashtbl.t =
  let size = List.length rules in
  let map = Hashtbl.create size in
  List.iter (fun ((x,y): (int * int)) ->
    let edges = Hashtbl.find_opt map x |> Option.value ~default:[] in
    Hashtbl.replace map x (y :: edges)
  ) rules;
  map


let is_valid_order (graph: (int, int list) Hashtbl.t) (update: int list): bool =
  let rec validate (visited: int list) (stack: int list) = function
    | [] -> true
    | x :: xs ->
      if List.mem x visited then
        false
      else
        let dependencies = Hashtbl.find_opt graph x |> Option.value ~default:[] in
        if List.exists (fun dep -> List.mem dep stack) dependencies then
          false
        else
          validate (x :: visited) (x :: stack) xs
    in
    validate [] [] update


let middle_page (update: int list): int =
  let n = List.length update in
  List.nth update (n/2)


let part1 (filename: string): unit =
  let rules, updates = parse_input filename in
  let graph = build_graph rules in
  let valid = List.filter (is_valid_order graph) updates in
  let mid = List.map middle_page valid in
  let res = List.fold_left ( + ) 0 mid in
  Printf.printf "%d\n" res


(* let topological_sort (graph: (int, int list) Hashtbl.t) (update:int list): int list = *)
  (* let visited = Hashtbl.create (List.length update) in *)
  (* let stack = ref [] in *)

  (* let rec visit (node: int) = *)
    (* if not (Hashtbl.mem visited node) then ( *)
      (* Hashtbl.add visited node true; *)
      (* let dependencies = Hashtbl.find_opt graph node |> Option.value ~default:[] in *)
      (* List.iter visit dependencies; *)
      (* stack := node :: !stack *) 
    (* ) *)
  (* in *)
  (* List.iter visit update; *)
  (* List.filter (fun x -> List.mem x update) !stack *)
  let topological_sort (graph: (int, int list) Hashtbl.t) (update: int list): int list =
  let in_degree = Hashtbl.create (List.length update) in
  
  (* Initialize in-degrees for all update nodes *)
  List.iter (fun node -> Hashtbl.add in_degree node 0) update;
  
  (* Calculate in-degrees by checking entire graph closure *)
  List.iter (fun src ->
    match Hashtbl.find_opt graph src with
    | Some dests ->
        List.iter (fun dest ->
          if List.mem dest update then
            let current = Hashtbl.find in_degree dest in
            Hashtbl.replace in_degree dest (current + 1)
        ) dests
    | None -> ()
  ) update;
  
  (* Queue for nodes with zero in-degree *)
  let queue = Queue.create () in
  List.iter (fun node ->
    if Hashtbl.find in_degree node = 0 then 
      Queue.add node queue
  ) update;
  
  (* Perform topological sort *)
  let result = ref [] in
  while not (Queue.is_empty queue) do
    let node = Queue.take queue in
    result := node :: !result;
    
    (* Reduce in-degrees of adjacent nodes *)
    match Hashtbl.find_opt graph node with
    | Some neighbors ->
        List.iter (fun neighbor ->
          if List.mem neighbor update then (
            let current_degree = Hashtbl.find in_degree neighbor in
            Hashtbl.replace in_degree neighbor (current_degree - 1);
            if current_degree = 1 then
              Queue.add neighbor queue
          )
        ) neighbors
    | None -> ()
  done;
  
  List.rev !result


let part2 (filename: string) : unit =
  let rules, updates = parse_input filename in
  let graph = build_graph rules in
  let incorrect = List.filter (fun update -> not (is_valid_order graph update)) updates in
  let corrected = List.map (fun update -> topological_sort graph update) incorrect in
  let mid = List.map middle_page corrected in
  let res = List.fold_left ( + ) 0 mid in
  Printf.printf "%d\n" res
