(* input_parser/day11.ml *)

open! Core
open! Util

let is_node_id (s : string) =
  String.length s = 3
  && String.for_all s ~f:(fun c -> Char.(c >= 'a' && c <= 'z'))
;;

(* Repo convention: little-endian UART words (LSB first) *)
let int64_to_uart_bytes_le (x : int64) : Uart_symbol.t list =
  List.init 8 ~f:(fun i ->
    let shifted = Int64.shift_right_logical x (i * 8) in
    let byte = (Int64.to_int_exn shifted) land 0xFF in
    Uart_symbol.Byte (Char.of_int_exn byte))
;;

let parse ?(verbose = false) (filename : string) : Uart_symbol.t list =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  (* Parse input lines into adjacency list by name *)
  let edges_str =
    raw
    |> String.split_lines
    |> List.filter_map ~f:(fun line ->
      let line =
        String.rstrip line ~drop:(fun c -> Char.(c = '\r'))
        |> String.strip
      in
      if String.is_empty line then None
      else (
        match String.lsplit2 line ~on:':' with
        | None -> failwithf "Day11: bad line (missing ':'): %S" line ()
        | Some (lhs, rhs) ->
          let src = String.strip lhs in
          if not (is_node_id src) then
            failwithf "Day11: bad node id: %S" src ();
          let kids =
            rhs
            |> String.strip
            |> (fun rhs ->
                 if String.is_empty rhs
                 then []
                 else
                   rhs
                   |> String.split ~on:' '
                   |> List.filter ~f:(Fn.non String.is_empty))
          in
          List.iter kids ~f:(fun k ->
            if not (is_node_id k) then
              failwithf "Day11: bad child id %S in %S" k line ());
          Some (src, kids)))
  in

  (* Collect all node names *)
  let all_nodes =
    edges_str
    |> List.concat_map ~f:(fun (s, ks) -> s :: ks)
    |> List.dedup_and_sort ~compare:String.compare
  in
  let n_all = List.length all_nodes in
  if n_all = 0 then failwith "Day11: empty graph";

  let name_of_id = Array.of_list all_nodes in
  let id_of_name =
    Map.of_alist_exn (module String)
      (Array.to_list name_of_id |> List.mapi ~f:(fun i n -> (n, i)))
  in

  (* Build adjacency on integer ids *)
  let adj_all = Array.create ~len:n_all [] in
  List.iter edges_str ~f:(fun (src, kids) ->
    let u = Map.find_exn id_of_name src in
    adj_all.(u) <- List.map kids ~f:(fun k -> Map.find_exn id_of_name k));

  (* Required nodes *)
  let you = "you" in
  let out = "out" in
  if not (Map.mem id_of_name you) then failwith "Day11: missing \"you\"";
  if not (Map.mem id_of_name out) then failwith "Day11: missing \"out\"";

  (* Optional part 2 nodes *)
  let svr = "svr" in
  let fft = "fft" in
  let dac = "dac" in
  let has_part2 =
    Map.mem id_of_name svr
    && Map.mem id_of_name fft
    && Map.mem id_of_name dac
  in

  (* Reachability pruning *)
  let reachable = Array.create ~len:n_all false in
  let bfs_from start =
    let q = Queue.create () in
    if not reachable.(start) then (
      reachable.(start) <- true;
      Queue.enqueue q start);
    while not (Queue.is_empty q) do
      let u = Queue.dequeue_exn q in
      List.iter adj_all.(u) ~f:(fun v ->
        if not reachable.(v) then (
          reachable.(v) <- true;
          Queue.enqueue q v))
    done
  in

  bfs_from (Map.find_exn id_of_name you);
  if has_part2 then bfs_from (Map.find_exn id_of_name svr);

  let reach_ids =
    List.init n_all ~f:Fn.id
    |> List.filter ~f:(fun i -> reachable.(i))
  in

  (* Kahn topological sort (deterministic: lexicographic tie-break) *)
  let indeg = Array.create ~len:n_all 0 in
  List.iter reach_ids ~f:(fun u ->
    List.iter adj_all.(u) ~f:(fun v ->
      if reachable.(v) then indeg.(v) <- indeg.(v) + 1));

  let ready =
    reach_ids
    |> List.filter ~f:(fun u -> indeg.(u) = 0)
    |> List.sort ~compare:(fun a b ->
      String.compare name_of_id.(a) name_of_id.(b))
    |> ref
  in

  let topo_rev = ref [] in
  while not (List.is_empty !ready) do
    let u = List.hd_exn !ready in
    ready := List.tl_exn !ready;
    topo_rev := u :: !topo_rev;

    List.iter adj_all.(u) ~f:(fun v ->
      if reachable.(v) then (
        indeg.(v) <- indeg.(v) - 1;
        if indeg.(v) = 0 then (
          ready :=
            (v :: !ready)
            |> List.sort ~compare:(fun a b ->
              String.compare name_of_id.(a) name_of_id.(b))
        )))
  done;

  let topo = List.rev !topo_rev in
  let num_nodes = List.length topo in

  let topo_arr = Array.of_list topo in
  let old_to_new = Array.create ~len:n_all (-1) in
  Array.iteri topo_arr ~f:(fun i u -> old_to_new.(u) <- i);

  let idx_of name =
    old_to_new.(Map.find_exn id_of_name name)
  in

  let idx_you = idx_of you in
  let idx_out = idx_of out in
  let idx_svr = if has_part2 then idx_of svr else 0 in
  let idx_fft = if has_part2 then idx_of fft else 0 in
  let idx_dac = if has_part2 then idx_of dac else 0 in

  (* Re-index adjacency into new topo indices *)
  let adj_new = Array.create ~len:num_nodes [] in
  Array.iter topo_arr ~f:(fun u ->
    let nu = old_to_new.(u) in
    adj_new.(nu) <-
      adj_all.(u)
      |> List.filter_map ~f:(fun v ->
        if reachable.(v) then Some (old_to_new.(v)) else None));

  (* Memory layout *)
  let header_words = 2 in
  let node_base = header_words in
  let edge_base0 = node_base + num_nodes in

  let header0 =
    let open Int64 in
    of_int num_nodes
    lor shift_left (of_int idx_you) 14
    lor shift_left (of_int idx_out) 28
    lor shift_left (of_int idx_svr) 42
    lor shift_left (of_int (Bool.to_int has_part2)) 56
  in

  let header1 =
    let open Int64 in
    of_int idx_fft
    lor shift_left (of_int idx_dac) 14
  in

  let edge_ptr = ref edge_base0 in
  let edge_heap = ref [] in

  let node_words =
    List.init num_nodes ~f:(fun u ->
      let kids = adj_new.(u) in
      let ecount = List.length kids in
      let ebase = !edge_ptr in
      edge_ptr := !edge_ptr + ecount;
      List.iter kids ~f:(fun v ->
        edge_heap := Int64.of_int v :: !edge_heap);
      let is_out = if u = idx_out then 1L else 0L in
      let open Int64 in
      shift_left (of_int ebase) 18
      lor shift_left (of_int ecount) 8
      lor is_out)
  in

  let words = header0 :: header1 :: (node_words @ List.rev !edge_heap) in

  words
  |> List.concat_map ~f:int64_to_uart_bytes_le
  |> fun xs -> xs @ [ Uart_symbol.Rts true ]
;;
