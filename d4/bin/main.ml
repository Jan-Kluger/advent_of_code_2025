module IntMap = Map.Make(Int) 

let rec parse_args ?(acc : string list = []) (file) =
  try
    let line = String.trim @@ input_line file in
    parse_args ~acc:(line :: acc) file
  with End_of_file -> acc

let is_some = function Some _ -> true | None -> false

let count_horizontal (row : int option list) : int option list =
  let rec aux prev rest acc =
    match rest with
    | [] -> List.rev acc
    | [curr] ->
      let result = match curr with
        | None -> None
        | Some _ -> Some (if is_some prev then 1 else 0)
      in
      List.rev (result :: acc)
    | curr :: (next :: _ as tail) ->
      let result = match curr with
        | None -> None
        | Some _ ->
          let left = if is_some prev then 1 else 0 in
          let right = if is_some next then 1 else 0 in
          Some (left + right)
      in
      aux curr tail (result :: acc)
  in
  aux None row []

let add_vertical_neighbors (adjacent : int option list) (counts : int option list) : int option list =
  let rec aux adj_prev adj_rest counts_rest acc =
    match adj_rest, counts_rest with
    | [], [] -> List.rev acc
    | [adj_curr], [count] ->
      let result = match count with
        | None -> None
        | Some c ->
          let up_left = if is_some adj_prev then 1 else 0 in
          let up = if is_some adj_curr then 1 else 0 in
          Some (c + up_left + up)
      in
      List.rev (result :: acc)
    | adj_curr :: (adj_next :: _ as adj_tail), count :: counts_tail ->
      let result = match count with
        | None -> None
        | Some c ->
          let up_left = if is_some adj_prev then 1 else 0 in
          let up = if is_some adj_curr then 1 else 0 in
          let up_right = if is_some adj_next then 1 else 0 in
          Some (c + up_left + up + up_right)
      in
      aux adj_curr adj_tail counts_tail (result :: acc)
    | _ -> acc
  in
  aux None adjacent counts []

let recount (grid : int option list list) : int option list list =
  match grid with
  | [] -> []
  | [single] -> [count_horizontal single]
  | first :: rest ->
    let first_counts = count_horizontal first in
    let (_, last_counts, result) =
      List.fold_left
        (fun (prev_row, prev_counts, acc) curr_row ->
          let prev_updated = add_vertical_neighbors curr_row prev_counts in
          let curr_counts = count_horizontal curr_row in
          let curr_with_up = add_vertical_neighbors prev_row curr_counts in
          (curr_row, curr_with_up, prev_updated :: acc))
        (first, first_counts, [])
        rest
    in
    List.rev (last_counts :: result)

let remove_accessible (grid : int option list list) : int option list list * int =
  List.fold_left (fun (rows_acc, total) row ->
    let (new_row, row_count) = List.fold_left (fun (row_acc, count) opt ->
      match opt with
      | Some v when v < 4 -> (None :: row_acc, count + 1)
      | x -> (x :: row_acc, count)
    ) ([], 0) row in
    (List.rev new_row :: rows_acc, total + row_count)
  ) ([], 0) grid |> fun (rows, count) -> (List.rev rows, count)

let rec remove_until_stable (grid : int option list list) : int option list list =
  let (after_remove, count) = remove_accessible grid in
  if count = 0 then grid
  else remove_until_stable (recount after_remove)

let input_path =
  if Sys.file_exists "bin/input.txt" then "bin/input.txt" else "input.txt"

let input = List.rev (parse_args (open_in input_path))

let initial_grid = List.map (fun line ->
    List.of_seq (Seq.map (fun c -> if c = '@' then Some 0 else None) (String.to_seq line))
  ) input

let c m = List.fold_left (fun acc x -> acc + List.fold_left(fun acc x ->
    match x with
    | None -> acc
    | _ -> acc + 1
  ) 0 x
  ) 0 m

module Solution1 = struct
  let solve grid =
    let lines = recount grid in
    List.fold_left (fun acc x -> acc + List.fold_left(fun acc x ->
        match x with
        | Some v when v < 4 -> acc + 1
        | _ -> acc
      ) 0 x) 0 lines
end

module Solution2 = struct
  let solve grid =
    let lines = recount grid in
    let after_remove = remove_until_stable lines in
    c grid - c after_remove
end

let () = print_endline (string_of_int (Solution1.solve initial_grid))
let () = print_endline (string_of_int (Solution2.solve initial_grid))
