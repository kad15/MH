(** An implementation of the LLL lattice basis reduction algorithm. *)

module B = Bignum.Std.Bignum
module P = Pervasives

exception NoSolution

val switch_array : 'a array -> int -> int -> unit
val switch_matrix : 'a array array -> int -> int -> int -> int -> unit
val dot_product : B.t array -> B.t array -> B.t
val gram_schmidt : B.t array array -> B.t array array * B.t array
val red : B.t array array -> B.t array array -> int -> int -> unit
val lll : B.t array array -> unit
val solve : B.t array -> B.t -> int array
