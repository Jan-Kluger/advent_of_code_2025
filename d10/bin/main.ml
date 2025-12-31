type machine = { n : int; target : int; buttons : int array }
type machine2 = { n2 : int; target2 : int array; buttons2 : int array array }

let to_mask s = String.to_seqi s |> Seq.fold_left (fun a (i, c) -> if c = '#' then a lor (1 lsl i) else a) 0
let parse_ints s = String.split_on_char ',' s |> List.map int_of_string

let extract_between line o c =
  let rec go i acc = match String.index_from_opt line i o with
    | None -> List.rev acc
    | Some s -> match String.index_from_opt line s c with
      | None -> List.rev acc
      | Some e -> go (e + 1) (String.sub line (s + 1) (e - s - 1) :: acc)
  in go 0 []

let parse_line line =
  let diagram = List.hd (extract_between line '[' ']') in
  let buttons = List.map parse_ints (extract_between line '(' ')') in
  let joltage = match extract_between line '{' '}' with [] -> [] | j :: _ -> parse_ints j in
  let btn_mask = List.fold_left (fun a x -> a lor (1 lsl x)) 0 in
  ({ n = String.length diagram; target = to_mask diagram; buttons = Array.of_list (List.map btn_mask buttons) },
   { n2 = List.length joltage; target2 = Array.of_list joltage; buttons2 = Array.of_list (List.map Array.of_list buttons) })

let parse_input ic =
  let rec loop acc = match input_line ic with
    | line when String.trim line <> "" -> loop (parse_line line :: acc)
    | _ -> loop acc | exception End_of_file -> List.rev acc
  in loop []

module Solution1 = struct
  module S = Set.Make(Int)
  let solve_machine { n = _; target; buttons } =
    let rec bfs vis = function
      | [] -> -1 | (st, d) :: _ when st = target -> d
      | (st, d) :: rest ->
        let next, v = Array.fold_left (fun (acc, v) m ->
          let s = st lxor m in if S.mem s v then (acc, v) else ((s, d+1) :: acc, S.add s v)
        ) ([], vis) buttons in bfs v (rest @ next)
    in bfs (S.singleton 0) [(0, 0)]
  let solve = List.map solve_machine
end

module Solution2 = struct
  let rec gcd a b = if b = 0 then abs a else gcd b (a mod b)

  let solve_machine { n2; target2; buttons2 } =
    if n2 = 0 then 0 else
    let nb = Array.length buttons2 in
    let mat = Array.init n2 (fun i -> Array.init (nb+1) (fun j ->
      if j = nb then target2.(i) else if Array.exists ((=) i) buttons2.(j) then 1 else 0)) in
    let pivot_col = Array.make n2 (-1) in
    (* Gaussian elimination *)
    let rec elim row col =
      if row >= n2 || col >= nb then row else
      let mr = Array.fold_left (fun m r -> if abs mat.(r).(col) > abs mat.(m).(col) then r else m) row (Array.init (n2-row) ((+) row)) in
      if mat.(mr).(col) = 0 then elim row (col+1) else begin
        let t = mat.(row) in mat.(row) <- mat.(mr); mat.(mr) <- t;
        pivot_col.(row) <- col;
        let pv = mat.(row).(col) in
        Array.iteri (fun r _ -> if r <> row && mat.(r).(col) <> 0 then begin
          let f = mat.(r).(col) in
          Array.iteri (fun c _ -> mat.(r).(c) <- mat.(r).(c) * pv - f * mat.(row).(c)) mat.(r);
          let g = Array.fold_left (fun a v -> if v = 0 then a else gcd a v) 0 mat.(r) in
          if g > 1 then Array.iteri (fun c v -> mat.(r).(c) <- v / g) mat.(r)
        end) mat;
        elim (row+1) (col+1)
      end
    in let rank = elim 0 0 in
    let is_piv = Array.make nb false in Array.iter (fun c -> if c >= 0 then is_piv.(c) <- true) pivot_col;
    let free = List.filter (fun c -> not is_piv.(c)) (List.init nb Fun.id) in
    let nf, maxv = List.length free, Array.fold_left max 0 target2 in
    let best = ref max_int in
    let check vals =
      let x = Array.make nb 0 in
      List.iteri (fun i c -> x.(c) <- List.nth vals (nf-1-i)) free;
      let ok = ref true in
      let rec back r = if r < 0 || not !ok then () else begin
        let pc = pivot_col.(r) in
        if pc >= 0 && mat.(r).(pc) <> 0 then begin
          let sum = List.fold_left (fun a (c,v) -> if c = pc then a else a - v * x.(c)) mat.(r).(nb)
            (Array.to_list (Array.mapi (fun c v -> (c,v)) mat.(r)) |> List.filter (fun (c,_) -> c < nb)) in
          if sum mod mat.(r).(pc) <> 0 then ok := false
          else (x.(pc) <- sum / mat.(r).(pc); if x.(pc) < 0 then ok := false)
        end; back (r-1)
      end in back (rank-1);
      if !ok && Array.for_all ((<=) 0) x then best := min !best (Array.fold_left (+) 0 x)
    in
    let rec search vals d =
      if d = nf then check vals
      else if List.fold_left (+) 0 vals < !best then
        List.iter (fun v -> search (v :: vals) (d+1)) (List.init (maxv+1) Fun.id)
    in
    if nf <= 3 then search [] 0 else check (List.init nf (fun _ -> 0));
    if !best = max_int then -1 else !best

  let solve = List.map solve_machine
end

let ic = open_in "input.txt"
let m1, m2 = List.split (parse_input ic)
let () = close_in ic
let () = print_endline @@ string_of_int (List.fold_left (+) 0 (Solution1.solve m1))
let () = print_endline @@ string_of_int (List.fold_left (+) 0 (Solution2.solve m2))
