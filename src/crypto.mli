(** Provides essential utilities for the Merkle-Hellman cryptosystem and
    some cryptanalysis methods.

    This module relies on
    {{:https://ocaml-batteries-team.github.io/batteries-included/hdoc2/BatIO.html} [BatIO]}
    to make the most of high-order abstract I/O.

    Internal functions are not shown below.
*)

type pub
(** The type for a public key *)
type priv
(** The type for a private key *)

exception KeyIO of string
exception KeySize of string
exception InvalidKey of string
exception InvalidInput of string
exception CorruptedBlock of string


(** {3 Generating, saving and loading keys} *)

val create_keys : int -> int -> int -> pub * priv
(** Create a pair of public and private keys of a given length.

    Usage: [Crypto.create_keys size d f]

    - [d] and [f] are two constants used to build the knapsack and affecting
      its density (see report).
      For a key of a given length, you may want a larger value for [d*f] to
      protect from a dynamic attack, and a lower value for [d*f] to protect
      from LLL (high density).

    Example: [let (pub_k, priv_k) = Crypto.create_keys 1500 20 2]
    @raise KeySize on [size <= 0]
*)

val save_pub_key : pub -> string -> unit
(** Write a public key to a file (created or overwritten).

    @raise KeyIO if writing fails
*)

val save_priv_key : priv -> string -> unit
(** Write a private key to a file (created or overwritten).

    @raise KeyIO if writing fails
*)

val save_keys : pub * string -> priv * string -> unit
(** [Crypto.save_keys (pub_k, "alice.pub") (priv_k, "alice")] is equivalent to :

[Crypto.save_pub_key pub_k "alice.pub";
Crypto.save_priv_key pub_k "alice"]
*)

val load_pub_key : string -> pub
(** Load a public key from a file.

    @raise KeyIO if reading fails
    @raise InvalidKey if the file is not a valid (public) key
*)

val load_priv_key : string -> priv
(** Load a private key from a file.

    @raise KeyIO if reading fails
    @raise InvalidKey if the file is not a valid (private) key
*)


(** {3 Channel encryption and decryption} *)

val encrypt_channel : pub -> BatIO.input -> 'a BatIO.output -> unit
(** Encrypt an input channel and write the result on an output channel
    block by block.

    Example: [
    let pub_k = Crypto.load_pub_key "bob.pub" in
    let cin = BatIO.input_string "Hello Bob <3" in
    let cout = BatIO.output_channel (open_out_bin "secret.enc") in
    Crypto.encrypt_channel pub_k cin cout]
*)

val decrypt_channel : priv -> BatIO.input -> 'a BatIO.output -> bool -> unit
(** Decrypt an input channel block by block and write the result on an output
    channel gradually.

    Usage: [Crypto.decrypt_channel priv_k cin cout no_color]

    - If a block is corrupted and cannot be decrypted, write [???]
      and go to the next block.
      Colors can be turned off (especially if the output is not [stdout])
      by setting [no_color] to [true].

    Example: [
    let priv_k = Crypto.load_priv_key "bob" in
    let cin = BatIO.input_channel (open_in_bin "secret.enc") in
    let cout = BatIO.stdout in
    Crypto.decrypt_channel priv_k cin cout false]

    {blank-line}
    @raise InvalidInput if the input is not an encrypted source or is too damaged
*)


(** {3 Cryptanalysis methods} *)
(** {C These methods {i try} to attack a channel with cryptanalysis methods which
       only rely on the public key. It might be computationally expensive and
       fall short of memory.} *)

val decrypt_channel_dynamic :
  pub -> BatIO.input -> 'a BatIO.output -> bool -> bool -> unit
(** Try to decrypt an input channel by using dynamic programming to solve the SSP.
    It is the least efficient method due to memory consumption.

    Usage: [Crypto.decrypt_channel pub_k cin cout flush no_color]

    - If a block is corrupted and cannot be decrypted, write [???]
      and go to the next block.
      Colors can be turned off (especially if the output is not [stdout])
      by setting [no_color] to [true].
    - Set [flush] to [true] to flush the output channel after each block
      is decrypted (useful to see the progress).

    @raise KeySize if it might or actually lack of memory
*)

val decrypt_channel_tree :
  pub -> BatIO.input -> 'a BatIO.output -> bool -> bool -> unit
(** Try to decrypt an input channel by using a tree+heuristic method.

    Usage: same as {!val:decrypt_channel_dynamic}
*)

val decrypt_channel_lll :
  pub -> BatIO.input -> 'a BatIO.output -> bool -> bool -> unit
(** Try to decrypt an input channel by using the LLL lattice basis
    reduction algorithm to solve the SSP. Success is not guaranteed and success
    probability decreases with the length and the density of the key.

    Usage: same as {!val:decrypt_channel_dynamic}
*)


(** {3 Utilities} *)

val fprint_pub : out_channel -> pub -> unit
(** Print a public key on an output channel.

    Example: [Crypto.fprint_pub stdout pub_k]
*)

val fprint_priv : out_channel -> priv -> unit
(** Print a private key on an output channel.

    Example: [Crypto.fprint_private stdout priv_k]
*)

val density_key : pub -> float
(** Compute the density of the knapsack (useful to predict LLL's performances). *)
