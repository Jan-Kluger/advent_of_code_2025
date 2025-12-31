module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)

module Solution1 = struct
  let solve (chl : in_channel) : int =
    let beam_set = IntSet.empty in
    
    let first_line = String.trim @@ input_line chl in
    let s_idx = String.index first_line 'S' in

    let init_set = IntSet.add s_idx beam_set in
    let init_line = String.trim @@ input_line chl in

    let rec compute_all_beams (line : string) (set : IntSet.t) : int =
      let l_length = String.length line in
      let splitter_idxs = fst @@ String.fold_left (fun ((iset, idx)) x ->
          match x with
          | '^' -> (idx :: iset, idx + 1)
          | _ -> (iset, idx + 1)
        ) ([],0) line
      in

      let cnt, beam_idx = List.fold_left (fun ((cnt, new_set) as acc) x ->
          match IntSet.find_opt x set with
          | Some v ->
            let left = max 0 (v - 1) in
            let right = min l_length (v + 1) in
            let new_set = IntSet.remove v new_set in
            let new_acc = IntSet.add right @@ IntSet.add left new_set in
            (cnt + 1, new_acc)
          | None -> acc
        ) (0, set) splitter_idxs
      in

      try
        let nline = String.trim @@ input_line chl in
        cnt + compute_all_beams nline beam_idx
      with End_of_file ->
        cnt
    in

    compute_all_beams init_line init_set
end

module Solution2 = struct
  let solve (chl : in_channel) : Z.t =
    let beam_counts = IntMap.empty in

    let add (k : int) (v : Z.t) (m : Z.t IntMap.t) : Z.t IntMap.t =
      IntMap.update k
        (function
          | None -> Some v
          | Some old -> Some (Z.add old v))
        m
    in

    let sum_counts (m : Z.t IntMap.t) : Z.t =
      IntMap.fold (fun _ v acc -> Z.add acc v) m Z.zero
    in

    let step_one_line (line : string) ((counts, exited) : Z.t IntMap.t * Z.t) :
        Z.t IntMap.t * Z.t =
      let l_length = String.length line in
      let splitter_idxs =
        fst
        @@ String.fold_left
             (fun (iset, idx) ch ->
               match ch with
               | '^' -> (idx :: iset, idx + 1)
               | _ -> (iset, idx + 1))
             ([], 0) line
      in

      let orig_counts = counts in
      List.fold_left
        (fun (new_counts, exited_acc) x ->
          match IntMap.find_opt x orig_counts with
          | None -> (new_counts, exited_acc)
          | Some c when Z.equal c Z.zero -> (new_counts, exited_acc)
          | Some c ->
            let new_counts = IntMap.remove x new_counts in
            let exited_acc, new_counts =
              if x - 1 < 0 then (Z.add exited_acc c, new_counts)
              else (exited_acc, add (x - 1) c new_counts)
            in
            let exited_acc, new_counts =
              if x + 1 >= l_length then (Z.add exited_acc c, new_counts)
              else (exited_acc, add (x + 1) c new_counts)
            in
            (new_counts, exited_acc))
        (counts, exited) splitter_idxs
    in

    try
      let first_line = String.trim @@ input_line chl in
      let s_idx = String.index first_line 'S' in
      let init_counts = IntMap.add s_idx Z.one beam_counts in
      let init_line = String.trim @@ input_line chl in

      let rec compute_all_timelines (line : string) (counts : Z.t IntMap.t) (exited : Z.t)
          : Z.t =
        let counts, exited = step_one_line line (counts, exited) in
        try
          let nline = String.trim @@ input_line chl in
          compute_all_timelines nline counts exited
        with End_of_file ->
          Z.add exited (sum_counts counts)
      in

      compute_all_timelines init_line init_counts Z.zero
    with End_of_file ->
      Z.zero
end

let input_path =
  if Sys.file_exists "input.txt" then "input.txt" else "bin/input.txt"

let input_file = open_in input_path
let () = Solution1.solve input_file |> string_of_int |> print_endline
let () = close_in input_file

let input_file2 = open_in input_path
let () = Z.to_string (Solution2.solve input_file2) |> print_endline
let () = close_in input_file2
