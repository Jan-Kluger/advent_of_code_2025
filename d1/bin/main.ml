module Solution1 = struct
  let rec solve ?(rotation : int = 50) ?(count : int = 0) (f : in_channel) : int =
    try
      let line = String.trim (input_line f) in
      if String.length line = 0 then solve ~rotation ~count f
      else
        let dir = line.[0] in
        let num = int_of_string (String.sub line 1 (String.length line - 1)) in
        let value = if dir = 'L' then -(num mod 100) else (num mod 100) in
        let new_rotation = (rotation + value) mod 100 in
        match new_rotation with
        | 0 -> solve ~rotation:(new_rotation) ~count:(count + 1) f
        | _ -> solve ~count ~rotation:(new_rotation) f
    with End_of_file ->
      close_in f;
      count
end

module Solution2 = struct
  let rec solve ?(rotation : int = 50) ?(count : int = 0) (f : in_channel) : int =
    try
      let line = String.trim (input_line f) in
      if String.length line = 0 then solve ~rotation ~count f
      else
        let dir = line.[0] in
        let num = int_of_string (String.sub line 1 (String.length line - 1)) in
        if num = 0 then solve ~count ~rotation f
        else
          let new_rotation = match dir with
            | 'L' -> ((rotation - (num mod 100)) mod 100 + 100) mod 100
            | _ -> (rotation + (num mod 100)) mod 100
          in
          let zero_crossings = match dir with
            | 'L' ->
              if rotation = 0 then num / 100
              else if num >= rotation then 1 + (num - rotation) / 100
              else 0
            | _ ->
              if rotation = 0 then num / 100
              else if num >= (100 - rotation) then 1 + (num - (100 - rotation)) / 100
              else 0
          in
          solve ~count:(count + zero_crossings) ~rotation:new_rotation f
    with End_of_file ->
      close_in f;
      count
end

let () = print_endline (string_of_int (Solution2.solve (open_in "input.txt")))
let () = print_endline (string_of_int (Solution1.solve (open_in "input.txt")))
