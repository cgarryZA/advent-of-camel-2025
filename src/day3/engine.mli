open Hardcaml

module Make : sig
  module I : sig
    type 'a t =
      { clock : 'a
      ; clear : 'a
      ; in_valid : 'a
      ; in_byte : 'a [@bits 8]
      ; in_last : 'a
      }
    [@@deriving hardcaml]
  end

  module O : sig
    type 'a t =
      { in_ready : 'a
      ; part1 : 'a [@bits 64]
      ; part2 : 'a [@bits 64]
      ; out_done : 'a
      }
    [@@deriving hardcaml]
  end

  val create : Scope.t -> Signal.t I.t -> Signal.t O.t
end

include module type of Make
