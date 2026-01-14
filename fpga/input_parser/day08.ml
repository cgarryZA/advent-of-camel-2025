(* input_parser/day08.ml *)

open! Core
open! Util

let max_points = 1024

let k_for_input ~n =
  (* Match your hardware convention:
     - sample (20 points) uses k=10
     - real (1000 points) uses k=1000 *)
  if n <= 20 then 10 else 1000
;;

module Uf = struct
  type t =
    { parent : int array
    ; size   : int array
    ; mutable comps : int
    }

  let create n =
    { parent = Array.init n ~f:Fn.id
    ; size   = Array.init n ~f:(fun _ -> 1)
    ; comps = n
    }
  ;;

  let rec find t x =
    let p = t.parent.(x) in
    if p = x then x else find t p
  ;;

  let union t a b =
    let ra = find t a in
    let rb = find t b in
    if ra = rb then false
    else begin
      if t.size.(ra) >= t.size.(rb) then begin
        t.parent.(rb) <- ra;
        t.size.(ra) <- t.size.(ra) + t.size.(rb);
      end else begin
        t.parent.(ra) <- rb;
        t.size.(rb) <- t.size.(rb) + t.size.(ra);
      end;
      t.comps <- t.comps - 1;
      true
    end
  ;;
end

let parse ?(verbose = false) (filename : string) : Uart_symbol.t list =
  let raw = get_input_file filename in
  if verbose then begin
    print_endline "==== RAW INPUT ====";
    print_endline raw;
  end;

  let points =
    raw
    |> String.split_lines
    |> List.filter_map ~f:(fun line ->
      let line = String.strip line in
      if String.is_empty line then None
      else
        let line = String.rstrip line ~drop:(Char.equal '\r') in
        match String.split line ~on:',' with
        | [ x; y; z ] ->
            let x = Int.of_string (String.strip x) in
            let y = Int.of_string (String.strip y) in
            let z = Int.of_string (String.strip z) in
            Some (x, y, z)
        | _ ->
            failwithf "Invalid line (expected x,y,z): %s" line ())
  in

  let n = List.length points in
  if n <= 0 then failwith "No points found in input";
  if n > max_points then
    failwithf "Too many points (%d). max_points=%d" n max_points ();

  if verbose then begin
    printf "Parsed %d points\n" n;
    List.iteri points ~f:(fun i (x, y, z) ->
      printf "  %04d: x=%d y=%d z=%d\n" i x y z);
  end;

  let xs = Array.of_list (List.map points ~f:(fun (x, _, _) -> x)) in

  (* Store coords as int64 for safe d2. *)
  let xa = Array.map xs ~f:Int64.of_int in
  let ya = Array.of_list (List.map points ~f:(fun (_, y, _) -> Int64.of_int y)) in
  let za = Array.of_list (List.map points ~f:(fun (_, _, z) -> Int64.of_int z)) in

  (* Build all edges (complete graph), then sort by (d2,u,v). *)
  let m = (n * (n - 1)) / 2 in
  let edges = Array.create ~len:m (Int64.zero, 0, 0) in
  let k = ref 0 in
  for i = 0 to n - 2 do
    for j = i + 1 to n - 1 do
      let dx = Int64.(xa.(i) - xa.(j)) in
      let dy = Int64.(ya.(i) - ya.(j)) in
      let dz = Int64.(za.(i) - za.(j)) in
      let d2 =
        Int64.(dx * dx + dy * dy + dz * dz)
      in
      edges.(!k) <- (d2, i, j);
      incr k
    done
  done;

  Array.sort edges ~compare:(fun (d2a, ua, va) (d2b, ub, vb) ->
    match Int64.compare d2a d2b with
    | 0 ->
        (match Int.compare ua ub with
         | 0 -> Int.compare va vb
         | c -> c)
    | c -> c);

  if verbose then begin
    printf "Built %d candidate edges\n" (Array.length edges);
  end;

  (* Find the first edge index where MST completes (n-1 unions). *)
  let uf = Uf.create n in
  let unions = ref 0 in
  let complete_at = ref 0 in
  let found = ref false in
  for i = 0 to Array.length edges - 1 do
    if not !found then begin
      let (_d2, u, v) = edges.(i) in
      if Uf.union uf u v then incr unions;
      if !unions = n - 1 then begin
        (* number of edges *consumed* to complete MST *)
        complete_at := i + 1;
        found := true
      end
    end
  done;

  let k_need = k_for_input ~n in
  let needed =
    Int.min (Array.length edges) (Int.max k_need !complete_at)
  in

  if verbose then begin
    printf "MST completes after %d edges (consumed). Streaming %d edges (k=%d).\n"
      !complete_at needed k_need;
  end;

  (* 1) xs preload (32-bit little-endian words) *)
  let xs_uart =
    Array.to_list xs
    |> List.concat_map ~f:(fun x -> int_to_uart_bytes_le ~n:4 x)
  in

  (* Delimiter between xs and edges: RTS pulse (true then false). *)
  let delim =
    [ Uart_symbol.Rts true
    ; Uart_symbol.Rts false
    ]
  in

  (* 2) edge stream: u16 LE, v16 LE (4 bytes per edge) *)
  let edges_uart =
    List.init needed ~f:(fun i -> edges.(i))
    |> List.concat_map ~f:(fun (_d2, u, v) ->
         let u_bytes = int_to_uart_bytes_le ~n:2 u in
         let v_bytes = int_to_uart_bytes_le ~n:2 v in
         u_bytes @ v_bytes)
  in

  xs_uart @ delim @ edges_uart
;;
