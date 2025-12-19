open! Hardcaml

val rows : int
val cols : int
val cells : int
val addr_bits : int

module I : sig
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; load_we : 'a
    ; load_addr : 'a [@bits addr_bits]
    ; load_data : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { ready : 'a
    ; p1 : 'a [@bits 32]
    ; p2 : 'a [@bits 32]
    ; finished : 'a
    }
  [@@deriving hardcaml]
end

val create : Scope.t -> Signal.t I.t -> Signal.t O.t
val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
