let rec parse_input (chl : in_channel) : (string * string list) list =
  try
    let line = String.trim @@ input_line chl in
    let s, t = match String.split_on_char ':' line with
      | state :: transitions -> (state, transitions)
      | _ -> failwith "ill formatted input"
    in
    (s, List.tl @@ String.split_on_char ' ' @@ List.hd t) :: parse_input chl
  with End_of_file -> []

module Solution1 = struct
  type node = string * string list

  module StringSet = Set.Make(String)

  let transition_table = Hashtbl.create 1024

  let solve (i : (string * string list) list) : int =
    List.iter (fun (key, vals) ->
        Hashtbl.add transition_table key vals;
      ) i;

    let init = match (List.find_opt (fun (k, _) -> k = "you") i) with
      | Some v -> v
      | _ -> failwith "no entry point defined"
    in

    let rec find_exit ?(visited : StringSet.t = StringSet.empty) ((_, e) : node) : int =
      match e with
      | [] -> 0
      | _ -> List.fold_left (fun acc x ->
          match x with
          | "out" -> acc + 1
          | _ ->
            match StringSet.find_opt x visited with
            | None ->
              let n_node = Hashtbl.find transition_table x in
              let n_visited = StringSet.add x visited in
              acc + find_exit ~visited:n_visited (x, n_node)
            | _ -> acc
        ) 0 e

    in

    find_exit init
end

module Solution2 = struct
  let solve (i : (string * string list) list) : int =
    let transition_table = Hashtbl.create 1024 in
    List.iter (fun (key, vals) -> Hashtbl.add transition_table key vals) i;

    let memo = Hashtbl.create 1024 in
    let in_progress = Hashtbl.create 1024 in

    let rec count_paths (node : string) (seen_dac : bool) (seen_fft : bool) : int =
      let key = (node, seen_dac, seen_fft) in
      match Hashtbl.find_opt memo key with
      | Some v -> v
      | None ->
        if Hashtbl.mem in_progress key then 0
        else begin
          Hashtbl.add in_progress key true;
          let successors = match Hashtbl.find_opt transition_table node with
            | Some v -> v
            | None -> []
          in
          let result = List.fold_left (fun acc succ ->
              let seen_dac' = seen_dac || (succ = "dac") in
              let seen_fft' = seen_fft || (succ = "fft") in
              if succ = "out" then
                acc + (if seen_dac' && seen_fft' then 1 else 0)
              else
                acc + count_paths succ seen_dac' seen_fft'
            ) 0 successors
          in
          Hashtbl.remove in_progress key;
          Hashtbl.add memo key result;
          result
        end
    in

    count_paths "svr" false false
end

let input = parse_input @@ open_in "input.txt"

let res = Solution1.solve input
let res2 = Solution2.solve input

let () = print_endline (string_of_int res)
let () = print_endline (string_of_int res2)
