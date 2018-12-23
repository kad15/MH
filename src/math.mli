(** Math utilities. *)

val miller_rabin_rounds : Z.t -> int
(** Return the number of rounds in the Miller-Rabin test to get an
    error probability [<= 2^-80].

   {i Reference: Handbook of applied cryptography }
*)

val probab_prime : Z.t -> bool
(** [probab_prime n] determines whether [n] is probably prime or definitely composite
   by using the Miller-Rabin test.
*)

val nextprime : Z.t -> Z.t
(** [nextprime n] returns the next probably prime number [>= n]. *)

val modinvert : Z.t -> Z.t -> Z.t
(** [modinvert a m] computes the modular multiplicative inverse of [a] modulo [m].
    @raise Failure if there is no solution
*)

(** Include Zarith and come with some additional functions. *)
module Z :
  sig
    (** {4 Zarith } *)
    type t = Z.t
    exception Overflow
    val zero : t
    val one : t
    val minus_one : t
    external of_int : int -> t = "ml_z_of_int" [@@noalloc]
    external of_int32 : int32 -> t = "ml_z_of_int32"
    external of_int64 : int64 -> t = "ml_z_of_int64"
    external of_nativeint : nativeint -> t = "ml_z_of_nativeint"
    external of_float : float -> t = "ml_z_of_float"
    val of_string : string -> t
    val of_substring : string -> pos:int -> len:int -> t
    val of_string_base : int -> string -> t
    external of_substring_base : int -> string -> pos:int -> len:int -> t
      = "ml_z_of_substring_base"
    external succ : t -> t = "ml_z_succ" "ml_as_z_succ"
    external pred : t -> t = "ml_z_pred" "ml_as_z_pred"
    external abs : t -> t = "ml_z_abs" "ml_as_z_abs"
    external neg : t -> t = "ml_z_neg" "ml_as_z_neg"
    external add : t -> t -> t = "ml_z_add" "ml_as_z_add"
    external sub : t -> t -> t = "ml_z_sub" "ml_as_z_sub"
    external mul : t -> t -> t = "ml_z_mul" "ml_as_z_mul"
    external div : t -> t -> t = "ml_z_div" "ml_as_z_div"
    external rem : t -> t -> t = "ml_z_rem" "ml_as_z_rem"
    external div_rem : t -> t -> t * t = "ml_z_div_rem"
    external cdiv : t -> t -> t = "ml_z_cdiv"
    external fdiv : t -> t -> t = "ml_z_fdiv"
    val ediv_rem : t -> t -> t * t
    val ediv : t -> t -> t
    val erem : t -> t -> t
    external divexact : t -> t -> t = "ml_z_divexact" "ml_as_z_divexact"
    external logand : t -> t -> t = "ml_z_logand" "ml_as_z_logand"
    external logor : t -> t -> t = "ml_z_logor" "ml_as_z_logor"
    external logxor : t -> t -> t = "ml_z_logxor" "ml_as_z_logxor"
    external lognot : t -> t = "ml_z_lognot" "ml_as_z_lognot"
    external shift_left : t -> int -> t = "ml_z_shift_left"
      "ml_as_z_shift_left"
    external shift_right : t -> int -> t = "ml_z_shift_right"
      "ml_as_z_shift_right"
    external shift_right_trunc : t -> int -> t = "ml_z_shift_right_trunc"
    external numbits : t -> int = "ml_z_numbits" [@@noalloc]
    external trailing_zeros : t -> int = "ml_z_trailing_zeros" [@@noalloc]
    val testbit : t -> int -> bool
    external popcount : t -> int = "ml_z_popcount"
    external hamdist : t -> t -> int = "ml_z_hamdist"
    external to_int : t -> int = "ml_z_to_int"
    external to_int32 : t -> int32 = "ml_z_to_int32"
    external to_int64 : t -> int64 = "ml_z_to_int64"
    external to_nativeint : t -> nativeint = "ml_z_to_nativeint"
    val to_float : t -> float
    val to_string : t -> string
    external format : string -> t -> string = "ml_z_format"
    external fits_int : t -> bool = "ml_z_fits_int" [@@noalloc]
    external fits_int32 : t -> bool = "ml_z_fits_int32" [@@noalloc]
    external fits_int64 : t -> bool = "ml_z_fits_int64" [@@noalloc]
    external fits_nativeint : t -> bool = "ml_z_fits_nativeint" [@@noalloc]
    val print : t -> unit
    val output : out_channel -> t -> unit
    val sprint : unit -> t -> string
    val bprint : Buffer.t -> t -> unit
    val pp_print : Format.formatter -> t -> unit
    external compare : t -> t -> int = "ml_z_compare" [@@noalloc]
    external equal : t -> t -> bool = "ml_z_equal" [@@noalloc]
    val leq : t -> t -> bool
    val geq : t -> t -> bool
    val lt : t -> t -> bool
    val gt : t -> t -> bool
    external sign : t -> int = "ml_z_sign" [@@noalloc]
    val min : t -> t -> t
    val max : t -> t -> t
    val is_even : t -> bool
    val is_odd : t -> bool
    external hash : t -> int = "ml_z_hash" [@@noalloc]
    external gcd : t -> t -> t = "ml_z_gcd"
    val gcdext : t -> t -> t * t * t
    val lcm : t -> t -> t
    external powm : t -> t -> t -> t = "ml_z_powm"
    external powm_sec : t -> t -> t -> t = "ml_z_powm_sec"
    external invert : t -> t -> t = "ml_z_invert"
    external probab_prime : t -> int -> int = "ml_z_probab_prime"
    external nextprime : t -> t = "ml_z_nextprime"
    external pow : t -> int -> t = "ml_z_pow"
    external sqrt : t -> t = "ml_z_sqrt"
    external sqrt_rem : t -> t * t = "ml_z_sqrt_rem"
    external root : t -> int -> t = "ml_z_root"
    external perfect_power : t -> bool = "ml_z_perfect_power"
    external perfect_square : t -> bool = "ml_z_perfect_square"
    val log2 : t -> int
    val log2up : t -> int
    external size : t -> int = "ml_z_size" [@@noalloc]
    external extract : t -> int -> int -> t = "ml_z_extract"
    val signed_extract : t -> int -> int -> t
    external to_bits : t -> string = "ml_z_to_bits"
    external of_bits : string -> t = "ml_z_of_bits"
    external ( ~- ) : t -> t = "ml_z_neg" "ml_as_z_neg"
    val ( ~+ ) : t -> t
    external ( + ) : t -> t -> t = "ml_z_add" "ml_as_z_add"
    external ( - ) : t -> t -> t = "ml_z_sub" "ml_as_z_sub"
    external ( * ) : t -> t -> t = "ml_z_mul" "ml_as_z_mul"
    external ( / ) : t -> t -> t = "ml_z_div" "ml_as_z_div"
    external ( /> ) : t -> t -> t = "ml_z_cdiv"
    external ( /< ) : t -> t -> t = "ml_z_fdiv"
    external ( /| ) : t -> t -> t = "ml_z_divexact" "ml_as_z_divexact"
    external ( mod ) : t -> t -> t = "ml_z_rem" "ml_as_z_rem"
    external ( land ) : t -> t -> t = "ml_z_logand" "ml_as_z_logand"
    external ( lor ) : t -> t -> t = "ml_z_logor" "ml_as_z_logor"
    external ( lxor ) : t -> t -> t = "ml_z_logxor" "ml_as_z_logxor"
    external ( ~! ) : t -> t = "ml_z_lognot" "ml_as_z_lognot"
    external ( lsl ) : t -> int -> t = "ml_z_shift_left" "ml_as_z_shift_left"
    external ( asr ) : t -> int -> t = "ml_z_shift_right"
      "ml_as_z_shift_right"
    external ( ~$ ) : int -> t = "ml_z_of_int" [@@noalloc]
    external ( ** ) : t -> int -> t = "ml_z_pow"
    val version : string

    (** {4 Additional values } *)
    val two : t
    val three : t
    val random : Zarith_1_4.Z.t -> Zarith_1_4.Z.t
    val to_bignum : Zarith_1_4.Z.t -> Bignum.Std.Bignum.t
    val output_array : out_channel -> t array -> unit
    val ( = ) : Z.t -> Z.t -> bool
    val ( > ) : Z.t -> Z.t -> bool
    val ( < ) : Z.t -> Z.t -> bool
    val ( >= ) : Z.t -> Z.t -> bool
    val ( <= ) : Z.t -> Z.t -> bool
    val ( <> ) : Z.t -> Z.t -> bool
  end
