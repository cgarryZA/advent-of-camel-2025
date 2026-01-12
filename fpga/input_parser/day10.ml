(* input_parser/day10.ml *)

open! Core
open! Util

(* Repo convention: loader shifts bytes MSB-first into a 64-bit word.
   So we must send bytes MSB-first per 64-bit word.
   int_to_uart_bytes_le returns LE order; reverse it for MSB-first. *)
let emit_u64 (x : int) : Uart_symbol.t list =
  int_to_uart_bytes_le ~n:8 x |> List.rev
;;

let parse ?(verbose = false) filename : Uart_symbol.t list =
  let raw = get_input_file filename in
  if verbose then print_endline raw;

  let lines =
    raw
    |> String.split_lines
    |> List.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
  in

  let machines =
    List.map lines ~f:(fun line ->
      (* Split at closing ']' of the light pattern *)
      let pattern, rest =
        match String.lsplit2 line ~on:']' with
        | Some (p, r) -> (p ^ "]", r)
        | None -> failwithf "Day10: malformed line (missing ']'): %S" line ()
      in

      (* Lights are inside [ ... ] *)
      let lights =
        pattern
        |> String.strip
        |> fun s ->
             if String.length s < 2 || not Char.(s.[0] = '[') then
               failwithf "Day10: bad pattern: %S" pattern ();
             String.sub s ~pos:1 ~len:(String.length s - 2)
      in
      let m = String.length lights in
      if m > 48 then
        failwithf "Day10: too many lights (%d). Hardware expects <= 48-ish." m ();

      (* target_mask uses bit i for light i (LSB = light 0) *)
      let target_mask =
        lights
        |> String.to_list
        |> List.foldi ~init:0 ~f:(fun i acc c ->
             match c with
             | '#' -> acc lor (1 lsl i)
             | '.' -> acc
             | _ -> failwithf "Day10: bad char in lights: %C" c ())
      in

      (* Buttons are (...) groups; each group lists indices to toggle. *)
      let button_masks =
        rest
        |> String.split ~on:'('
        |> List.tl
        |> Option.value ~default:[]
        |> List.map ~f:(fun s ->
             match String.lsplit2 s ~on:')' with
             | None -> failwithf "Day10: malformed button group in: %S" line ()
             | Some (body, _) ->
               body
               |> String.strip
               |> String.split ~on:','
               |> List.filter ~f:(Fn.non String.is_empty)
               |> List.map ~f:(fun x ->
                    let i = Int.of_string (String.strip x) in
                    if i < 0 || i >= m then
                      failwithf "Day10: button index %d out of range (m=%d) in %S" i m line ();
                    i))
        |> List.map ~f:(fun idxs ->
             List.fold idxs ~init:0 ~f:(fun acc i -> acc lor (1 lsl i)))
      in

      let k = List.length button_masks in
      if k > 255 then failwithf "Day10: too many buttons (%d)" k ();

      (* Header word layout (same as RTL expects):
         bits  7:0  = m_lights
         bits 15:8  = k_buttons
         bits 63:16 = target_mask (LSB=light0) *)
      let header =
        m lor (k lsl 8) lor (target_mask lsl 16)
      in

      header, button_masks)
  in

  let stream_words =
    emit_u64 (List.length machines)
    :: List.concat_map machines ~f:(fun (hdr, masks) ->
         emit_u64 hdr :: List.map masks ~f:emit_u64)
    |> List.concat
  in

  stream_words @ [ Uart_symbol.Rts true ]
;;
