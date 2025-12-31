let rec get_lines ?(acc : int list list = []) (f : in_channel) : int list list =
  try
    let line = String.trim (input_line f) in
    if String.length line = 0 then get_lines ~acc f
    else
      let line_nums : int list = String.fold_left (fun acc x -> (int_of_string(Char.escaped x)) :: acc) [] line in
      get_lines ~acc:(List.rev(line_nums) :: acc) f
  with End_of_file ->
    close_in f;
    acc

module Solution1 = struct
  let rec work (i : int list) : (int * int) =
    match i with
    | x :: y :: [] -> (x, y)
    | x :: xs ->
      let ox, oy = work xs in
      let value a b = 10*a + b in
      List.fold_left (fun (ax, ay) (nx,ny) -> if value ax ay > value nx ny then (ax, ay) else (nx,ny)) (0,0) [(x, ox) ; (x , oy) ; (ox, oy)]
    | _ -> failwith "shouldnt happen"

  let solve t =
    let res = List.rev @@ List.fold_left (fun acc x -> (work x) :: acc) [] t in
    List.fold_left (fun acc (t,b) -> acc + 10*t + b) 0 res
end

module Solution2 = struct
  let work_2 digits =
    let n = List.length digits in
    let to_remove = n - 12 in
    let rec build_stack stack removed remaining =
      match remaining with
      | [] -> stack
      | d :: rest ->
        let rec pop_smaller stk rem =
          if rem >= to_remove then (stk, rem)
          else match stk with
            | top :: rest_stk when d > top -> pop_smaller rest_stk (rem + 1)
            | _ -> (stk, rem)
        in
        let new_stack, new_removed = pop_smaller stack removed in
        build_stack (d :: new_stack) new_removed rest
    in
    let stack = build_stack [] 0 digits in
    let result_digits = List.rev stack in
    let rec take n lst = 
      if n = 0 then [] 
      else match lst with [] -> [] | x :: xs -> x :: take (n-1) xs 
    in
    let first_12 = take 12 result_digits in
    List.fold_left (fun acc d -> acc * 10 + d) 0 first_12

  let solve t =
    List.fold_left (fun acc x -> work_2 x + acc) 0 t
end

let t = List.rev @@ get_lines (open_in "input.txt")

let () = print_endline (string_of_int (Solution1.solve t))
let () = print_endline (string_of_int (Solution2.solve t))
