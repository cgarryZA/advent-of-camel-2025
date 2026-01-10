open! Core
open! Util

let parse ?(verbose = false) filename =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  (* Emit exactly one 64-bit word - MUST be MSB-first to match Util.shift_in *)
  let emit_u64 (x : int) =
    int_to_uart_bytes_le ~n:8 x |> List.rev
  in

  let lines =
    raw
    |> String.split_lines
    |> List.filter ~f:(Fn.non String.is_empty)
  in

  let machines =
    List.map lines ~f:(fun line ->
      let pattern, rest =
        match String.lsplit2 line ~on:']' with
        | Some (p, r) -> p ^ "]", r
        | None -> failwith "Malformed input line"
      in

      (* Lights *)
      let lights =
        pattern
        |> String.strip
        |> fun s -> String.sub s ~pos:1 ~len:(String.length s - 2)
      in
      let m = String.length lights in

      let target_mask =
        lights
        |> String.to_list
        |> List.foldi ~init:0 ~f:(fun i acc c ->
             if Char.(c = '#') then acc lor (1 lsl i) else acc)
      in

      (* Buttons *)
      let button_masks =
        rest
        |> String.split ~on:'('
        |> List.tl_exn
        |> List.map ~f:(fun s ->
             match String.lsplit2 s ~on:')' with
             | Some (body, _) ->
                 body
                 |> String.strip
                 |> String.split ~on:','
                 |> List.filter ~f:(Fn.non String.is_empty)
                 |> List.map ~f:Int.of_string
             | None -> failwith "Malformed button")
        |> List.map ~f:(fun idxs ->
             List.fold idxs ~init:0 ~f:(fun acc i ->
               acc lor (1 lsl i)))
      in

      let k = List.length button_masks in

      (* Header word:
         bits  7:0  = m_lights
         bits 15:8  = k_buttons
         bits 63:16 = target_mask *)
      let header =
        m
        lor (k lsl 8)
        lor (target_mask lsl 16)
      in

      header, button_masks)
  in

  let stream =
    emit_u64 (List.length machines)
    :: List.concat_map machines ~f:(fun (hdr, masks) ->
         emit_u64 hdr :: List.map masks ~f:emit_u64)
    |> List.concat
  in

  stream @ [ Uart_symbol.Rts true ]
;;
