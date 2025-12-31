module IntMap = Map.Make (struct type t = int let compare = compare end)

let rec rest chl =
  try let num = int_of_string @@ input_line chl in
    num :: (rest chl)
  with End_of_file -> []

let rec parse_args ?(acc : (int -> bool) = (fun _ -> false)) (chl : in_channel) : ((int-> bool) * (int list)) =
  let line = input_line chl in
  begin
    match String.split_on_char '-' line with
    | x :: y :: _ ->
      let l, u = int_of_string x, int_of_string y in
      let new_fun = (fun i -> (i >= l && i <= u) || acc i ) in
      parse_args ~acc:(new_fun) chl
    | _ -> (acc, rest chl)
  end

let parse_ranges chl =
  let rec parse acc =
    try
      let line = input_line chl in
      match String.split_on_char '-' line with
      | x :: y :: _ ->
        let l, u = int_of_string x, int_of_string y in
        (* If start already exists, keep the maximum end value *)
        let end_val = try max u (IntMap.find l acc) with Not_found -> u in
        parse (IntMap.add l end_val acc)
      | _ -> acc  (* Stop at blank line, but return accumulated ranges *)
    with End_of_file -> acc
  in
  parse IntMap.empty

let merge_and_count ranges =
  let (total, last_range) = IntMap.fold (fun start end_ (acc, curr_range) ->
    match curr_range with
    | None -> (acc, Some (start, end_))
    | Some (curr_start, curr_end) ->
      if start <= curr_end + 1 then
        (* Overlapping or adjacent: extend current range *)
        (acc, Some (curr_start, max curr_end end_))
      else
        (* Not overlapping: count current range and start new one *)
        (acc + (curr_end - curr_start + 1), Some (start, end_))
  ) ranges (0, None) in
  match last_range with
  | None -> total
  | Some (start, end_) -> total + (end_ - start + 1)

module Solution1 = struct
  let solve () =
    let chl = open_in "bin/input.txt" in
    let f, ls = parse_args chl in
    close_in chl;
    List.fold_left (fun acc x -> if f x then acc + 1 else acc) 0 ls
end

module Solution2 = struct
  let solve () =
    let chl = open_in "bin/input.txt" in
    let ranges = parse_ranges chl in
    close_in chl;
    merge_and_count ranges
end

let () = Printf.printf "Part 1: %d\n" (Solution1.solve ())
let () = Printf.printf "Part 2: %d\n" (Solution2.solve ())
