module IntMap = Map.Make (struct type t = int let compare = compare end)

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

let test_ranges = 
  let m = IntMap.empty in
  let m = IntMap.add 3 5 m in
  let m = IntMap.add 10 14 m in
  let m = IntMap.add 16 20 m in
  let m = IntMap.add 12 18 m in
  m

let () = 
  let result = merge_and_count test_ranges in
  Printf.printf "Test result: %d (expected 14)\n" result;
  (* Should be: 3,4,5,10,11,12,13,14,15,16,17,18,19,20 = 14 IDs *)
  IntMap.iter (fun s e -> Printf.printf "Range: %d-%d\n" s e) test_ranges


