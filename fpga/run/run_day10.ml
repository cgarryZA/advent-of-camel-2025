open! Core
open! Util

(* Repo convention: little-endian UART words (LSB first) *)
let int64_to_uart_bytes_le (x : int64) : Uart_symbol.t list =
  List.init 8 ~f:(fun i ->
    let shifted = Int64.shift_right_logical x (i * 8) in
    let byte = (Int64.to_int_exn shifted) land 0xFF in
    Uart_symbol.Byte (Char.of_int_exn byte))
;;

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  (* Emit exactly one 64-bit word - MUST be MSB-first to match Util.shift_in *)
  let emit_u64 (x : int64) =
    int64_to_uart_bytes_le x |> List.rev
  in

  let lines =
    raw
    |> String.split_lines
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
  in

  let machines =
    List.map lines ~f:(fun line ->
      let pattern, rest =
        match String.lsplit2 line ~on:']' with
        | Some (p, r) -> p ^ "]", r
        | None -> failwith "Day10 parser: malformed input line (missing ']')"
      in

      (* Lights *)
      let lights =
        pattern
        |> String.strip
        |> fun s ->
             if String.length s < 2 then failwith "Day10 parser: empty [] pattern";
             String.sub s ~pos:1 ~len:(String.length s - 2)
      in
      let m = String.length lights in
      if m <= 0 then failwith "Day10 parser: m_lights must be > 0";
      if m > 63 then failwith "Day10 parser: m_lights > 63 not supported by this encoding";

      let m_mask =
        (* low m bits set *)
        Int64.(sub (shift_left 1L m) 1L)
      in

      let target_mask =
        lights
        |> String.to_list
        |> List.foldi ~init:0L ~f:(fun i acc c ->
             if Char.(c = '#')
             then Int64.(logxor acc (shift_left 1L i))
             else acc)
        |> fun t -> Int64.(logand t m_mask)
      in

      (* Buttons *)
      let button_masks =
        rest
        |> String.split ~on:'('
        |> List.tl
        |> Option.value ~default:[]
        |> List.map ~f:(fun s ->
             match String.lsplit2 s ~on:')' with
             | Some (body, _) ->
                 body
                 |> String.strip
                 |> String.split ~on:','
                 |> List.filter ~f:(fun x -> not (String.is_empty (String.strip x)))
                 |> List.map ~f:(fun x -> Int.of_string (String.strip x))
             | None -> failwith "Day10 parser: malformed button (missing ')')")
        |> List.map ~f:(fun idxs ->
             idxs
             |> List.fold ~init:0L ~f:(fun acc i ->
                  if i < 0 || i >= m then
                    failwithf "Day10 parser: button index %d out of range [0,%d)" i m ();
                  (* XOR (parity), not OR *)
                  Int64.(logxor acc (shift_left 1L i)))
             |> fun bm -> Int64.(logand bm m_mask))
      in

      let k = List.length button_masks in
      if k > 16 then
        failwithf "Day10 parser: k_buttons=%d > 16 (hardware core searches up to 16)" k ();

      (* Header word:
         bits  7:0  = m_lights
         bits 15:8  = k_buttons
         bits 63:16 = target_mask (masked to m bits) *)
      let header =
        let open Int64 in
        (of_int m)
        lor shift_left (of_int k) 8
        lor shift_left target_mask 16
      in

      header, button_masks)
  in

  let stream =
    emit_u64 (Int64.of_int (List.length machines))
    :: List.concat_map machines ~f:(fun (hdr, masks) ->
         emit_u64 hdr :: List.map masks ~f:emit_u64)
    |> List.concat
  in

  stream @ [ Uart_symbol.Rts true ]
;;
