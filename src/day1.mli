open! Core
open! Hardcaml

module I : sig
  type 'a t =
    { clock     : 'a
    ; clear     : 'a
    ; start     : 'a
    ; finish    : 'a
    ; direction : 'a
    ; steps     : 'a
    ; valid     : 'a
    }
  [@@deriving hardcaml]
end

module O : sig
  type 'a t =
    { ready    : 'a
    ; p1       : 'a
    ; p2       : 'a
    ; finished : 'a
    }
  [@@deriving hardcaml]
end

val hierarchical : Scope.t -> Signal.t I.t -> Signal.t O.t
