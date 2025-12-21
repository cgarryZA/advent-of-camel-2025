open! Core

let format_cycles (n : int) : string =
  (* Force underscore grouping (not commas), regardless of Core settings. *)
  let s = Int.to_string n in
  let sign, digits =
    if String.is_prefix s ~prefix:"-" then "-", String.drop_prefix s 1 else "", s
  in
  let len = String.length digits in
  let buf = Buffer.create (len + (len / 3)) in
  for i = 0 to len - 1 do
    if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char buf '_';
    Buffer.add_char buf digits.[i]
  done;
  sign ^ Buffer.contents buf
;;

let print ~cycles ~dt_s =
  printf "Cycles: %s\n" (format_cycles cycles);
  printf "Time  : %.3fms\n" (dt_s *. 1000.0)
;;
