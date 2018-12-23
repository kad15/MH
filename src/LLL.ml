open Bignum.Std
module B = Bignum
module P = Pervasives

exception NoSolution
exception SolutionFound of B.t array

let switch_array a i i' =
  let temp = a.(i) in
  a.(i) <- a.(i');
  a.(i') <- temp

let switch_matrix m i j i' j' =
  let temp = m.(i).(j) in
  m.(i).(j) <- m.(i').(j');
  m.(i').(j') <- temp

let dot_product u v =
  let s = ref B.zero in
  for i = 0 to Array.length u - 1 do
    s := B.(!s + u.(i) * v.(i))
  done;
  !s

let gram_schmidt b =
  let n = Array.length b in
  let b_orth = Array.make n [||] in
  let b_norm = Array.make n B.zero in
  let mu = Array.make_matrix n n B.zero in
  b_orth.(0) <- b.(0);
  b_norm.(0) <- dot_product b_orth.(0) b_orth.(0);
  for i = 1 to n - 1 do
    b_orth.(i) <- b.(i);
    for j = 0 to i - 1 do
      mu.(i).(j) <- B.(dot_product b.(i) b_orth.(j) / b_norm.(j));
      b_orth.(i) <- Array.mapi (fun k x -> B.(x - mu.(i).(j) * b_orth.(j).(k))) b_orth.(i)
    done;
    b_norm.(i) <- dot_product b_orth.(i) b_orth.(i)
  done;
  (mu, b_norm)


let red b mu k l =
  if B.(abs mu.(k).(l) > of_float 0.5001) then begin
    let r = B.(round ?dir:(Some `Down) (of_float 0.5 + mu.(k).(l))) in
    b.(k) <- Array.mapi (fun i x -> B.(x - r * b.(l).(i))) b.(k);
    for j = 0 to l - 1 do
      mu.(k).(j) <- B.(mu.(k).(j) - r * mu.(l).(j))
    done;
    mu.(k).(l) <- B.(mu.(k).(l) - r)
  end

let lll b =
  let n = Array.length b in
  let (mu, b_norm) = gram_schmidt b in
  let k = ref 1 in
  while !k <= n - 1 do
    red b mu !k (!k - 1);
    while B.(b_norm.(!k) < (of_float 0.75 - mu.(!k).(P.(!k - 1)) ** 2) * b_norm.(P.(!k - 1))) do
      let mu_ = mu.(!k).(!k - 1) in
      let b_ = B.(b_norm.(!k) + (mu_ ** 2) * b_norm.(P.(!k - 1))) in
      mu.(!k).(!k - 1) <- B.(mu_ * b_norm.(P.(!k - 1)) / b_);
      b_norm.(!k) <- B.(b_norm.(!k) * b_norm.(P.(!k - 1)) / b_);
      b_norm.(!k - 1) <- b_;
      switch_array b !k (!k - 1);
      if !k > 1 then
        for j = 0 to !k - 2 do
          switch_matrix mu !k j (!k - 1) j
        done;
      for i = !k + 1 to n - 1 do
        let t = mu.(i).(!k) in
        mu.(i).(!k) <- B.(mu.(i).(P.(!k - 1)) - mu_ * t);
        mu.(i).(!k - 1) <- B.(t + mu.(!k).(P.(!k - 1)) * mu.(i).(!k))
      done;
      k := max 1 (!k - 1);
      red b mu !k (!k - 1)
    done;
    for l = !k - 2 downto 0 do
      red b mu !k l
    done;
    incr k
  done

(* Use Coster, La Macchia, Odlyzko, Schnorr algorithm
   to compute lattice basis reduction, in order to try to
   find a solution for the SSP (`weights`, `total_sum`). *)
let solve weights total_sum =
  let n = Array.length weights in
  let b = Array.make_matrix (n + 1) (n + 1) B.zero in
  let m = B.of_float (ceil (0.5 *. sqrt (float n))) in
  (* build the lattice basis `b` *)
  for i = 0 to n - 1 do
    b.(i).(i) <- B.one;
    b.(i).(n) <- B.(m * weights.(i));
    b.(n).(i) <- B.of_float 0.5
  done;
  b.(n).(n) <- B.(m * total_sum);

  lll b;  (* apply LLL on `b` *)

  (* find if there is a solution in the new basis *)
  let x1 = Array.make n B.zero in
  let x2 = Array.make n B.zero in
  let i = ref 0 in
  try
    while !i < n + 1 do
      if B.(b.(!i).(n) = zero) then begin
        let j = ref 0 in
        while !j < n && B.(abs b.(!i).(!j) = of_float 0.5 || b.(!i).(!j) = zero) do
          x1.(!j) <- B.(of_float 0.5 + b.(!i).(!j));
          x2.(!j) <- B.(of_float 0.5 - b.(!i).(!j));
          incr j
        done;
        if !j = n then
          if dot_product x1 weights = total_sum then
            raise (SolutionFound x1)
          else if dot_product x2 weights = total_sum then
            raise (SolutionFound x2)
      end;
      incr i
    done;
    raise NoSolution
  with SolutionFound x ->
    Array.map B.to_int_exn x
