module UnionFind : sig
  type t
  val create : int -> t
  val union  : t -> int -> int -> bool
  val sizes  : t -> int list
end = struct
  type t = {
    parent : int array;
    size   : int array;
  }

  let create n =
    { parent = Array.init n Fun.id;
      size   = Array.make n 1 }

  let rec find uf x =
    let p = uf.parent.(x) in
    if p = x then x
    else begin
      let r = find uf p in
      uf.parent.(x) <- r;
      r
    end

  let union uf x y =
    let rx = find uf x
    and ry = find uf y in
    if rx = ry then false
    else begin
      if uf.size.(rx) < uf.size.(ry) then begin
        uf.parent.(rx) <- ry;
        uf.size.(ry) <- uf.size.(ry) + uf.size.(rx)
      end else begin
        uf.parent.(ry) <- rx;
        uf.size.(rx) <- uf.size.(rx) + uf.size.(ry)
      end;
      true
    end

  let sizes uf =
    let seen = Hashtbl.create 16 in
    Array.iteri (fun i _ ->
      let r = find uf i in
      if not (Hashtbl.mem seen r) then
        Hashtbl.add seen r uf.size.(r)
    ) uf.parent;
    Hashtbl.to_seq_values seen |> List.of_seq
end

let rec parse (chl : in_channel) : (int * int * int) list =
  try
    let line = String.trim @@ input_line chl in
    let split = String.split_on_char ',' line in
    match split with
    | x ::  y :: z :: _ ->
      let c = int_of_string in
      (c x, c y, c z) :: parse chl
    | _ -> failwith "ill formatted input"
  with End_of_file -> []

module Solution1 = struct
  let connect_until
      (until : int)
      (points : (int * int * int) list)
    : UnionFind.t =
    let pts = Array.of_list points in
    let n = Array.length pts in
    if n < 2 then invalid_arg "connect_until";

    let sq x = x * x in
    let dist2 (x1,y1,z1) (x2,y2,z2) =
      sq (x1 - x2) + sq (y1 - y2) + sq (z1 - z2)
    in

    let edges = ref [] in
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        edges := (dist2 pts.(i) pts.(j), i, j) :: !edges
      done
    done;

    let edges =
      List.sort (fun (d1,_,_) (d2,_,_) -> compare d1 d2) !edges
    in

    let uf = UnionFind.create n in

    let rec loop connected = function
      | [] -> ()
      | _ when connected = until -> ()
      | (_, i, j) :: es ->
          ignore (UnionFind.union uf i j);
          loop (connected + 1) es
    in
    loop 0 edges;
    uf

  let solve (points : (int * int * int) list) : int =
    let uf = connect_until 1000 points in
    let sizes =
      UnionFind.sizes uf
      |> List.sort compare |> List.rev
    in
    match sizes with
    | a :: b :: c :: _ -> a * b * c
    | _ -> failwith "not getting enough different unions?"
end

module Solution2 = struct
  let solve (points : (int * int * int) list) : int =
    let pts = Array.of_list points in
    let n = Array.length pts in
    if n < 2 then invalid_arg "solve_part2";

    let sq x = x * x in
    let dist2 (x1,y1,z1) (x2,y2,z2) =
      sq (x1 - x2) + sq (y1 - y2) + sq (z1 - z2)
    in

    let rec gen_edges i j acc =
      if i >= n - 1 then acc
      else if j >= n then gen_edges (i + 1) (i + 2) acc
      else gen_edges i (j + 1) ((dist2 pts.(i) pts.(j), i, j) :: acc)
    in
    let edges = gen_edges 0 1 [] in

    let edges = List.sort (fun (d1,_,_) (d2,_,_) -> compare d1 d2) edges in

    let uf = UnionFind.create n in
    let target = n - 1 in

    let rec loop unions = function
      | [] -> failwith "ran out of edges before fully connected"
      | (_, i, j) :: es ->
          if UnionFind.union uf i j then
            let unions' = unions + 1 in
            if unions' = target then
              let (x1, _, _) = pts.(i) in
              let (x2, _, _) = pts.(j) in
              x1 * x2
            else
              loop unions' es
          else
            loop unions es
    in
    loop 0 edges
end

let input_path = "input.txt"

let res = parse @@ open_in input_path

let part1 = Solution1.solve res
let part2 = Solution2.solve res

let () =
  Printf.printf "Part 1: %d\nPart 2: %d\n" part1 part2
