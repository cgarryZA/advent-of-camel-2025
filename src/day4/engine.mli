open! Core
open! Hardcaml

module Make (P : sig
  val lanes : int
  val rows : int
  val cols : int
end) : sig
  val lanes : int
  val rows : int
  val cols : int

  val ext_rows : int
  val ext_cols : int

  val words_per_row : int
  val stride : int
  val words_total : int

  val load_addr_bits : int
  val count_bits : int

  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; start : 'a
      ; finish : 'a
      ; load_we : 'a
      ; load_addr : 'a [@bits load_addr_bits]
      ; load_word : 'a [@bits lanes]
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { ready : 'a
      ; p1 : 'a [@bits count_bits]
      ; p2 : 'a [@bits count_bits]
      ; finished : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
  val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
end [@@warning "-67"]

module Default :
  module type of Make (struct
    let lanes = 64
    let rows = 135
    let cols = 135
  end)

include module type of Default
