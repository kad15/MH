let _ = Random.self_init ()

let state = Core_kernel.Std.Random.State.make_self_init ()

module Z = struct
  include Z

  let two = succ one
  let three = succ two

  let random n =
    Bignum.Std.Bigint.(of_zarith_bigint n |> random ~state:state |> to_zarith_bigint)

  let to_bignum n=
    Bignum.Std.(Bigint.of_zarith_bigint n |> Bignum.of_bigint)

  let output_array chan a =
    let n = Pervasives.pred (Array.length a) in
    output_char chan '[';
    Array.iteri (fun i x ->
      output chan x;
      if i <> n then output_char chan ' '
    ) a;
    output_char chan ']'

  let ( = )  = Z.equal
  let ( > )  = Z.gt
  let ( < )  = Z.lt
  let ( >= ) = Z.geq
  let ( <= ) = Z.leq
  let ( <> ) x y = not (Z.equal x y)
end


(* Number of rounds in the Miller-Rabin test to get an
   error probability <= 2^-80.
   Reference: Handbook of applied cryptography *)
let miller_rabin_rounds n =
  let nb = Z.numbits n in
  if nb >= 1300 then 2
  else if nb >= 850 then 3
  else if nb >= 650 then 4
  else if nb >= 350 then 8
  else if nb >= 250 then 12
  else if nb >= 150 then 18
  else 27

(* Determine whether `n` is probably prime or definitely composite
   by using the Miller-Rabin test.
   Zarith already comes with a `probab_prime` function. *)
let probab_prime n =
  if Z.is_even n then n = Z.two
  else if n = Z.three then true
  else
    let n_1 = Z.pred n in
    (* we write n-1 = 2^s*r *)
    let s = ref 0 in
    let r = ref n_1 in
    while Z.is_even !r do
      r := Z.(!r / two);
      incr s
    done;

    let try_composite a =
      let y = ref (Z.powm_sec a !r n) in  (* compute y = a^r mod n *)
      if Z.(!y = one || !y = n_1) then false
      else begin
        let is_composite = ref true in
        let j = ref 1 in
        while !is_composite && !j < !s do
          y := Z.(powm_sec !y two n); (* ie a^(2^j)*r mod n *)
          if !y = n_1 then is_composite := false;
          incr j
        done;
        !is_composite
      end in

    let prob_prime = ref true in
    let i = ref 0 in
    let k = miller_rabin_rounds n in
    while !prob_prime && !i < k do
      (* choose a random `a` in [2, n-2] *)
      let a = Z.(two + (random (n - three))) in
      prob_prime := not (try_composite a);
      incr i
    done;
    !prob_prime

(* Return the next probably prime number >= `n`.
   Zarith already comes with a `nextprime` function. *)
let rec nextprime n =
  if Z.is_even n then nextprime Z.(succ n)
  else if probab_prime n then n
  else nextprime Z.(n + two)


(* Compute the modular multiplicative inverse of `a` modulo `m`.
   Zarith already comes with a `invert` function. *)
let modinvert a m =
  let rec egcd a b =
    if Z.(a = zero) then (b, Z.zero, Z.one)
    else
      let (g, v, u) = egcd Z.(b mod a) a in
      let q = Z.(b / a) in
      (g, Z.(u - q * v), v) in

  let (g, u, v) = egcd a m in
  if Z.(g <> one) then failwith "No modular inverse"
  else Z.(u + m)
