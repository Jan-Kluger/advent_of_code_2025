module IntMap = Map.Make(Int)

module Solution1 = struct
  let solve ic =
    let content = In_channel.input_all ic in
    let lines = String.split_on_char '\n' content in
    let coords = List.filter_map (fun s ->
        match String.split_on_char ',' s with
        | [x; y] -> Some (int_of_string y, int_of_string x)
        | _ -> None) lines in
    let rows = List.fold_left (fun acc (y, x) ->
        let lo, hi = match List.assoc_opt y acc with
          | Some (l, h) -> min l x, max h x
          | None -> x, x in
        (y, (lo, hi)) :: List.remove_assoc y acc) [] coords in
    List.fold_left (fun best (y1, (lo1, hi1)) ->
      List.fold_left (fun best (y2, (lo2, hi2)) ->
        if y1 >= y2 then best
        else max best ((max (hi1 - lo2) (hi2 - lo1) + 1) * (y2 - y1 + 1))
      ) best rows
    ) 0 rows
end

module Solution2 = struct
  let parse_coords lines =
    List.filter_map (fun s ->
      match String.split_on_char ',' s with
      | [x; y] -> Some (int_of_string x, int_of_string y)
      | _ -> None) lines

  let build_vsegs reds =
    let arr = Array.of_list reds in
    let n = Array.length arr in
    let indices = List.init n Fun.id in
    List.filter_map (fun i ->
        let x1, y1 = arr.(i) and x2, y2 = arr.((i + 1) mod n) in
        if x1 = x2 && y1 <> y2 then
          Some (x1, min y1 y2, max y1 y2, if y2 > y1 then 1 else -1)
        else None) indices

  let compress coords =
    let sorted = List.sort_uniq compare coords in
    let tbl = Hashtbl.create (List.length sorted) in
    let final_idx, _ = List.fold_left (fun (idx, prev) v ->
      let idx = if prev >= 0 && v > prev + 1 then idx + 1 else idx in
      Hashtbl.add tbl v idx;
      idx + 1, v
    ) (0, -1) sorted in
    tbl, final_idx

  let fill_row vsegs to_cx y =
    let filtered = List.filter_map (fun (x, y_lo, y_hi, dir) ->
        if y_lo <= y && y <= y_hi then Some (Hashtbl.find to_cx x, dir)
        else None) vsegs in
    let sorted = List.sort compare filtered in
    let _, _, acc = List.fold_left (fun (cx, inside, acc) (bx, dir) ->
        let filled = if inside <> 0 
          then List.init (bx - cx) (fun i -> cx + i) @ acc 
          else acc in
        bx + 1, inside + dir, bx :: filled
      ) (0, 0, []) sorted in
    acc

  let fill_polygon vsegs to_cx to_cy n_cy =
    let ys_list = Hashtbl.fold (fun y cy acc -> (cy, y) :: acc) to_cy [] in
    let sorted_ys = Array.of_list (List.sort compare ys_list) in
    let cy_to_y = Array.make n_cy 0 in
    Array.iteri (fun i (cy, y) ->
      cy_to_y.(cy) <- y;
      if i > 0 then
        let prev_cy, prev_y = sorted_ys.(i - 1) in
        for c = prev_cy + 1 to cy - 1 do cy_to_y.(c) <- (prev_y + y) / 2 done
    ) sorted_ys;
    let indices = List.init n_cy Fun.id in
    List.fold_left (fun grid cy ->
        let row_cxs = fill_row vsegs to_cx cy_to_y.(cy) in
        List.fold_left (fun g cx ->
            IntMap.update cx (function
              | None -> Some (IntMap.singleton cy true)
              | Some m -> Some (IntMap.add cy true m)) g
          ) grid row_cxs
      ) IntMap.empty indices

  let build_prefix grid n_cx n_cy =
    let arr = Array.init (n_cx + 1) (fun i ->
      Array.init (n_cy + 1) (fun j ->
        if i = 0 || j = 0 then 0
        else
          let v = match IntMap.find_opt (i-1) grid with
            | Some m -> if IntMap.mem (j-1) m then 1 else 0
            | None -> 0 in
          v)) in
    for i = 0 to n_cx - 1 do
      for j = 0 to n_cy - 1 do
        arr.(i+1).(j+1) <- arr.(i+1).(j+1) + arr.(i).(j+1) + arr.(i+1).(j) - arr.(i).(j)
      done
    done;
    arr

  let rect_filled prefix cx1 cy1 cx2 cy2 =
    prefix.(cx2+1).(cy2+1) - prefix.(cx1).(cy2+1) - prefix.(cx2+1).(cy1) + prefix.(cx1).(cy1)
    = (cx2 - cx1 + 1) * (cy2 - cy1 + 1)

  let group_by_y coords =
    List.fold_left (fun acc (x, y) ->
      IntMap.update y (function
        | None -> Some [x]
        | Some xs -> Some (x :: xs)) acc
    ) IntMap.empty coords

  let solve ic =
    let content = In_channel.input_all ic in
    let lines = String.split_on_char '\n' content in
    let reds = parse_coords lines in
    let vsegs = build_vsegs reds in
    let to_cx, n_cx = compress (List.map fst reds) in
    let to_cy, n_cy = compress (List.map snd reds) in
    let grid = fill_polygon vsegs to_cx to_cy n_cy in
    let prefix = build_prefix grid n_cx n_cy in
    let rows = IntMap.bindings (group_by_y reds) in
    List.fold_left (fun best (y1, xs1) ->
      let cy1 = Hashtbl.find to_cy y1 in
      List.fold_left (fun best (y2, xs2) ->
        if y2 <= y1 then best
        else
          let cy2 = Hashtbl.find to_cy y2 in
          List.fold_left (fun best x1 ->
            List.fold_left (fun best x2 ->
              let cx1, cx2 = Hashtbl.find to_cx (min x1 x2),
                             Hashtbl.find to_cx (max x1 x2) in
              if rect_filled prefix cx1 cy1 cx2 cy2
              then max best ((abs (x2 - x1) + 1) * (y2 - y1 + 1))
              else best
            ) best xs2
          ) best xs1
      ) best rows
    ) 0 rows
end

let () =
  let res1 = Solution1.solve (open_in "input.txt") in
  let res2 = Solution2.solve (open_in "input.txt") in
  print_endline (string_of_int res1);
  print_endline (string_of_int res2)
