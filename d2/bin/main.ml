module IntSet = Set.Make (Int)

let parse_input filename =
  let input = open_in filename in
  let s = input_line input in
  close_in input;
  let chars = List.of_seq (String.to_seq s) in
  let curr, past, tuples =
    List.fold_left
      (fun (curr, past, acc) x ->
        match x with
        | '-' -> ("", curr, acc)
        | ',' -> ("", "", (int_of_string past, int_of_string curr) :: acc)
        | c -> (curr ^ String.make 1 c, past, acc))
      ("", "", []) chars
  in
  if past <> "" && curr <> "" then
    (int_of_string past, int_of_string curr) :: tuples
  else tuples

let pow10 n = int_of_float (10.0 ** float_of_int n)

let range a b =
  let rec aux acc i = if i < a then acc else aux (i :: acc) (i - 1) in
  aux [] b

module Solution1 = struct
  let sum_invalid_in_range lo hi =
    let rec process n acc =
      let pow10_n = pow10 n in
      let multiplier = pow10_n + 1 in
      let min_x = if n = 1 then 1 else pow10_n / 10 in
      let max_x = pow10_n - 1 in
      let min_invalid = min_x * multiplier in
      if min_invalid > hi then acc
      else
        let x_lo = max min_x ((lo + multiplier - 1) / multiplier) in
        let x_hi = min max_x (hi / multiplier) in
        let sum =
          if x_lo > x_hi then 0
          else
            let count = x_hi - x_lo + 1 in
            multiplier * count * (x_lo + x_hi) / 2
        in
        process (n + 1) (acc + sum)
    in
    process 1 0

  let solve ranges =
    List.fold_left (fun acc (lo, hi) -> acc + sum_invalid_in_range lo hi) 0 ranges
end

module Solution2 = struct
  let sum_invalid_in_range_2 lo hi =
    let max_digits = String.length (string_of_int hi) + 1 in

    let collect_for_k p pow10_p min_x max_x k =
      let total_digits = p * k in
      if total_digits > max_digits then None
      else
        let pow10_total = pow10 total_digits in
        let multiplier = (pow10_total - 1) / (pow10_p - 1) in
        let min_invalid = min_x * multiplier in
        if min_invalid > hi then None
        else
          let x_lo = max min_x ((lo + multiplier - 1) / multiplier) in
          let x_hi = min max_x (hi / multiplier) in
          let ids =
            range x_lo x_hi
            |> List.map (fun x -> x * multiplier)
            |> List.filter (fun id -> id >= lo && id <= hi)
          in
          Some ids
    in

    let rec process_k p pow10_p min_x max_x k acc =
      match collect_for_k p pow10_p min_x max_x k with
      | None -> acc
      | Some ids ->
          let acc' = List.fold_left (Fun.flip IntSet.add) acc ids in
          process_k p pow10_p min_x max_x (k + 1) acc'
    in

    let rec process_p p acc =
      if p > max_digits / 2 then acc
      else
        let pow10_p = pow10 p in
        let min_x = if p = 1 then 1 else pow10_p / 10 in
        let max_x = pow10_p - 1 in
        let acc' = process_k p pow10_p min_x max_x 2 acc in
        process_p (p + 1) acc'
    in

    let invalid_ids = process_p 1 IntSet.empty in
    IntSet.fold ( + ) invalid_ids 0

  let solve ranges =
    List.fold_left (fun acc (lo, hi) -> acc + sum_invalid_in_range_2 lo hi) 0 ranges
end

let () =
  let ranges = parse_input "input.txt" in
  let total1 = Solution1.solve ranges in
  let total2 = Solution2.solve ranges in
  Printf.printf "Part 1: %d\n" total1;
  Printf.printf "Part 2: %d\n" total2
