type cell = int * int
type shape = cell list
type board = { width : int; height : int }
type region = { board : board; requirements : int list }
type mask = int
type piece = int * mask list

let trim (s : string) : string =
  String.concat "" (String.split_on_char '\r' (String.trim s))

let parse_cells (lines : string list) : shape =
  let parse_line (r : int) (line : string) : cell list =
    let seq = String.to_seq line in
    let indexed = Seq.mapi (fun c ch -> (r, c, ch)) seq in
    let hashes = Seq.filter (fun (_, _, ch) -> ch = '#') indexed in
    let coords = Seq.map (fun (r, c, _) -> (r, c)) hashes in
    List.of_seq coords
  in
  List.concat (List.mapi parse_line lines)

let parse (ic : in_channel) : shape list * region list =
  let content = In_channel.input_all ic in
  let raw_lines = String.split_on_char '\n' content in
  let lines = List.map trim raw_lines in
  let shapes = Hashtbl.create 10 in
  let flush (cur : (int * string list) option) : unit =
    match cur with
    | Some (idx, ls) -> Hashtbl.add shapes idx (parse_cells (List.rev ls))
    | None -> ()
  in
  let rec go (regions : region list) (cur : (int * string list) option) (lines : string list) : region list =
    match lines with
    | [] ->
      flush cur;
      regions
    | "" :: rest ->
      flush cur;
      go regions None rest
    | line :: rest ->
      match String.split_on_char ':' line with
      | [dims; counts] when String.contains dims 'x' ->
        flush cur;
        let w, h = Scanf.sscanf dims "%dx%d" (fun w h -> (w, h)) in
        let trimmed = trim counts in
        let parts = String.split_on_char ' ' trimmed in
        let non_empty = List.filter (fun s -> s <> "") parts in
        let reqs = List.map int_of_string non_empty in
        let region = { board = { width = w; height = h }; requirements = reqs } in
        go (region :: regions) None rest
      | [idx; ""] ->
        flush cur;
        go regions (Some (int_of_string idx, [])) rest
      | _ ->
        let cur' =
          match cur with
          | Some (idx, ls) -> Some (idx, line :: ls)
          | None -> None
        in
        go regions cur' rest
  in
  let regions = List.rev (go [] None lines) in
  let max_idx = Hashtbl.fold (fun k _ acc -> max k acc) shapes (-1) in
  let shapes =
    List.init (max_idx + 1) (fun i ->
      match Hashtbl.find_opt shapes i with
      | Some cells -> cells
      | None -> [])
  in
  (shapes, regions)

module Solution1 = struct
  let normalize (cells : shape) : shape =
    match cells with
    | [] -> []
    | _ ->
      let min_r = List.fold_left (fun acc (r, _) -> min acc r) max_int cells in
      let min_c = List.fold_left (fun acc (_, c) -> min acc c) max_int cells in
      let shifted = List.map (fun (r, c) -> (r - min_r, c - min_c)) cells in
      List.sort compare shifted

  let rotate (cells : shape) : shape =
    let rotated = List.map (fun (r, c) -> (c, -r)) cells in
    normalize rotated

  let flip (cells : shape) : shape =
    let flipped = List.map (fun (r, c) -> (r, -c)) cells in
    normalize flipped

  let all_orientations (cells : shape) : shape list =
    let r0 = normalize cells in
    let r1 = rotate r0 in
    let r2 = rotate r1 in
    let r3 = rotate r2 in
    let f0 = flip r0 in
    let f1 = flip r1 in
    let f2 = flip r2 in
    let f3 = flip r3 in
    List.sort_uniq compare [r0; r1; r2; r3; f0; f1; f2; f3]

  let bounding_height (cells : shape) : int =
    1 + List.fold_left (fun acc (r, _) -> max acc r) 0 cells

  let bounding_width (cells : shape) : int =
    1 + List.fold_left (fun acc (_, c) -> max acc c) 0 cells

  let to_mask (width : int) (cells : shape) : mask =
    List.fold_left (fun m (r, c) -> m lor (1 lsl (r * width + c))) 0 cells

  let placements (board : board) (cells : shape) : mask list =
    let { width; height } = board in
    let place_orientation (orient : shape) : mask list =
      let bh = bounding_height orient in
      let bw = bounding_width orient in
      let max_dr = max 0 (height - bh + 1) in
      let max_dc = max 0 (width - bw + 1) in
      let place (dr : int) (dc : int) : mask =
        let shifted = List.map (fun (r, c) -> (r + dr, c + dc)) orient in
        to_mask width shifted
      in
      let place_row (dr : int) : mask list =
        List.map (fun dc -> place dr dc) (List.init max_dc Fun.id)
      in
      List.concat_map place_row (List.init max_dr Fun.id)
    in
    List.concat_map place_orientation (all_orientations cells)

  let search (pieces : piece list) : bool =
    let rec go (used : mask) (pieces : piece list) : bool =
      match pieces with
      | [] -> true
      | _ ->
        let count_valid ((id, masks) : piece) : int * int * mask list =
          let valid = List.filter (fun m -> used land m = 0) masks in
          (List.length valid, id, valid)
        in
        let ranked = List.sort compare (List.map count_valid pieces) in
        match ranked with
        | (0, _, _) :: _ -> false
        | (_, id, valid) :: _ ->
          let others = List.filter (fun (i, _) -> i <> id) pieces in
          List.exists (fun m -> go (used lor m) others) valid
        | [] -> true
    in
    go 0 pieces

  let can_fit (shapes : shape list) (region : region) : bool =
    let { board; requirements } = region in
    let area (s : shape) : int = List.length s in
    let required_area = List.fold_left2 (fun acc s c -> acc + area s * c) 0 shapes requirements in
    let total_count = List.fold_left ( + ) 0 requirements in
    let board_area = board.width * board.height in
    let blocks_3x3 = (board.width / 3) * (board.height / 3) in
    if required_area > board_area then
      false
    else if blocks_3x3 >= total_count then
      true
    else
      let make_pieces (si : int) (count : int) : piece list =
        let shape = List.nth shapes si in
        let masks = placements board shape in
        List.init count (fun i -> (si * 1000 + i, masks))
      in
      let pieces = List.concat (List.mapi make_pieces requirements) in
      search pieces

  let solve (shapes : shape list) (regions : region list) : int =
    List.length (List.filter (can_fit shapes) regions)
end

let shapes, regions = parse (open_in "input.txt")
let res = Solution1.solve shapes regions

let () = print_endline (string_of_int res)
