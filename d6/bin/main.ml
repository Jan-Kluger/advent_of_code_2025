let read_lines_raw (chl : in_channel) : string list =
  let rec go acc =
    match input_line chl with
    | line -> go (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  go []

let is_op_char (c : char) : bool =
  c = '+' || c = '-' || c = '*' || c = '/'

(* ---------- Shared helpers ---------- *)

let parse_part1_lines (lines : string list) : string list list =
  let parse_line line =
    let line = String.trim line in
    let parts = String.split_on_char ' ' line in
    List.fold_left
      (fun acc x -> match x with "" -> acc | _ -> x :: acc)
      [] parts
  in
  let rows = List.map parse_line lines in
  let rec drop_trailing_empty = function
    | [] -> []
    | xs ->
      (match List.rev xs with
       | [] -> []
       | [] :: rest_rev -> drop_trailing_empty (List.rev rest_rev)
       | _ -> xs)
  in
  drop_trailing_empty rows

let get_top (i : 'a list list) : ('a list * 'a list list) =
  let (a, b) =
    List.fold_left
      (fun (t, r) x ->
         match x with
         | xh :: xs -> (xh :: t, xs :: r)
         | [] -> (t, r))
      ([], []) i
  in
  (a, List.rev b)

let eval_list (i : string list) : int =
  match i with
  | [] -> failwith "empty input"
  | op :: args ->
    let int x =
      try int_of_string x
      with Failure _ -> failwith ("int_of_string failed on: " ^ x ^ " (op=" ^ op ^ ")")
    in
    match op with
    | "+" -> List.fold_left (fun acc x -> acc + int x) 0 args
    | "-" -> List.fold_left (fun acc x -> acc - int x) 0 args
    | "*" -> List.fold_left (fun acc x -> acc * int x) 1 args
    | "/" -> List.fold_left (fun acc x -> acc / int x) 1 args
    | _ -> failwith "not a valid operator"

let rec get_vals_part1 (i : string list list) : int list =
  let t, r = get_top i in
  match t with
  | [] -> []
  | _ ->
    let v = eval_list t in
    v :: get_vals_part1 r

let pad_right_to (n : int) (s : string) : string =
  let len = String.length s in
  if len >= n then s
  else (
    let b = Bytes.create n in
    Bytes.blit_string s 0 b 0 len;
    for i = len to n - 1 do
      Bytes.set b i ' '
    done;
    Bytes.unsafe_to_string b
  )

let char_at (s : string) (i : int) : char =
  if i < 0 then ' ' else if i >= String.length s then ' ' else s.[i]

let col_is_blank (rows : string list) (i : int) : bool =
  List.for_all (fun row -> char_at row i = ' ') rows

let split_problems_by_columns_right_to_left (rows : string list) : int list list =
  let width = List.fold_left (fun acc s -> max acc (String.length s)) 0 rows in
  let rows = List.map (pad_right_to width) rows in
  let rec loop i curr acc =
    if i < 0 then
      let acc = if curr = [] then acc else (List.rev curr) :: acc in
      List.rev acc
    else if col_is_blank rows i then
      let acc = if curr = [] then acc else (List.rev curr) :: acc in
      loop (i - 1) [] acc
    else
      loop (i - 1) (i :: curr) acc
  in
  loop (width - 1) [] []

let digits_in_column (rows_without_op : string list) (i : int) : string option =
  let digits_rev =
    List.fold_left
      (fun acc row ->
         let c = char_at row i in
         if c >= '0' && c <= '9' then c :: acc else acc)
      [] rows_without_op
  in
  match List.rev digits_rev with
  | [] -> None
  | cs ->
    let len = List.length cs in
    let b = Bytes.create len in
    let rec fill j = function
      | [] -> ()
      | c :: rest ->
        Bytes.set b j c;
        fill (j + 1) rest
    in
    fill 0 cs;
    Some (Bytes.unsafe_to_string b)

let parse_problem_part2 (rows : string list) (col_indices : int list) : (string * string list) =
  let rec find_op_row_from_bottom rev_rows =
    match rev_rows with
    | [] -> failwith "empty input"
    | row :: rest ->
      if List.exists (fun i -> is_op_char (char_at row i)) col_indices
      then (row, List.rev rest)
      else find_op_row_from_bottom rest
  in
  let (op_row, rows_without_op) = find_op_row_from_bottom (List.rev rows) in

  let rec find_op = function
    | [] -> failwith "no operator column found"
    | i :: rest ->
      let c = char_at op_row i in
      if is_op_char c then c else find_op rest
  in
  let op_char = find_op col_indices in
  let op = String.make 1 op_char in

  let nums =
    col_indices
    |> List.fold_left
         (fun acc i ->
            match digits_in_column rows_without_op i with
            | None -> acc
            | Some s -> s :: acc)
         []
    |> List.rev
  in
  (op, nums)

(* ---------- Solution modules ---------- *)

module Solution1 = struct
  let solve (lines : string list) : int =
    let grid1 = parse_part1_lines lines in
    get_vals_part1 grid1 |> List.fold_left ( + ) 0
end

module Solution2 = struct
  let solve (lines : string list) : int =
    let problems = split_problems_by_columns_right_to_left lines in
    let debug = match Sys.getenv_opt "DEBUG_PART2" with Some "1" -> true | _ -> false in
    let vals =
      problems
      |> List.mapi (fun idx cols ->
           let (op, nums) = parse_problem_part2 lines cols in
           if debug then
             Printf.printf "problem[%d] op=%s nums=[%s]\n%!" idx op (String.concat "," nums);
           eval_list (op :: nums))
    in
    List.fold_left ( + ) 0 vals
end

(* ---------- main ---------- *)

let input_path = "input.txt"

let () =
  let ch = open_in input_path in
  let lines = read_lines_raw ch in
  close_in ch;
  let res1 = Solution1.solve lines in
  let res2 = Solution2.solve lines in
  Printf.printf "Part 1: %d\n" res1;
  Printf.printf "Part 2: %d\n" res2
