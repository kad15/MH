open Math

type pub  = {n: int; seq: Z.t array}
type priv = {n: int; seq: Z.t array; p: Z.t; a: Z.t; a_inv: Z.t}
(* we will need static typing where type inference will fail… that's a choice! *)

exception KeyIO of string
exception KeySize of string
exception InvalidKey of string
exception InvalidInput of string
exception CorruptedBlock of string


let superincr_gen cumulative_sum d i =
  let delta = Z.of_int (1 + Random.int d) in
  let elt = Z.(!cumulative_sum + delta) in
  cumulative_sum := Z.(!cumulative_sum + elt);
  elt

let create_keys n d f =
  if n <= 0 then raise (KeySize "Keys must have a length > 0");
  if d <= 0 then failwith "d must be >= 1";
  if f <= 0 then failwith "f must be >= 1";

  let cumulative_sum = ref Z.zero in
  let private_seq = Array.init n (superincr_gen cumulative_sum d) in
  let p_delta =
    if f > 1 then let f = f - 1 in Z.(random (of_int f * !cumulative_sum))
    else Z.zero in
  let p = nextprime Z.(!cumulative_sum + one + p_delta) in
  let a = Z.(one + random (pred p)) in  (* 0 < a < p *)
  let a_inv = modinvert a p in
  let public_seq = Array.init n (fun i -> Z.(a * private_seq.(i) mod p)) in

  let pub_k  = {n = n; seq = public_seq} in
  let priv_k = {n = n; seq = private_seq; p = p; a = a; a_inv = a_inv} in
  (pub_k, priv_k)

(* Compute the density of the knapsack - useful to predict LLL's performances *)
let density_key (pub_k:pub) =
  let den = Array.fold_left (fun acc x -> max acc (Z.log2 x)) 0 pub_k.seq in
  float pub_k.n /. float den

let fprint_pub cout (pub_k:pub) =
  Printf.fprintf cout "n = %d\nseq = %a\ndensity (LLL) = %f\n"
      pub_k.n
      Z.output_array pub_k.seq
      (density_key pub_k)

let fprint_priv cout (priv_k:priv) =
  Printf.fprintf cout "n = %d\nseq = %a\np = %a\na = %a\na_inv = %a\n"
      priv_k.n
      Z.output_array priv_k.seq
      Z.output priv_k.p
      Z.output priv_k.a
      Z.output priv_k.a_inv

let save_pub_key (pub_k:pub) path =
  try
    let chan = open_out_bin path in
    Marshal.to_channel chan ("pub", pub_k) [];
    close_out chan
  with Sys_error _ -> raise (KeyIO ("Cannot write public key in "^path))

let save_priv_key (priv_k:priv) path =
  try
    let chan = open_out_bin path in
    Marshal.to_channel chan ("priv", priv_k) [];
    close_out chan
  with Sys_error _ -> raise (KeyIO ("Cannot write private key in "^path))

let save_keys (pub_k, pub_path) (priv_k, priv_path) =
  save_pub_key pub_k pub_path;
  save_priv_key priv_k priv_path

let load_key path =
  try
    let chan = open_in_bin path in
    let (t, key) = Marshal.from_channel chan in
    close_in chan;
    (t, key)
  with
    | Sys_error _ -> raise (KeyIO ("Cannot read "^path))
    | Failure _ -> raise (InvalidKey (path^" is not a valid key"))

let load_pub_key path : pub =
  let (t, key) = load_key path in
  if t = "pub" then key
  else raise (InvalidKey (path^" is not a valid public key"))

let load_priv_key path : priv =
  let (t, key) = load_key path in
  if t = "priv" then key
  else raise (InvalidKey (path^" is not a valid private key"))


(* Compute dot product of the public key and a binary block. *)
let encrypt_block (pub_k:pub) block_bin =
  let block_sum = ref Z.zero in
  for i = 0 to pub_k.n - 1 do
    if block_bin.(i) = 1 then
      block_sum := Z.(!block_sum + pub_k.seq.(i))
  done;
  !block_sum

(* Decrypt a block by solving a greedy, superincreasing knapsack. *)
let decrypt_block (priv_k:priv) block_sum =
  let inv_sum = Z.(priv_k.a_inv * block_sum mod priv_k.p) in
  let block_bin = Array.make priv_k.n 1 in
  let s = ref inv_sum in
  for i = priv_k.n - 1 downto 0 do
    if Z.(!s >= priv_k.seq.(i)) then
      s := Z.(!s - priv_k.seq.(i))
    else block_bin.(i) <- 0
  done;
  if Z.(!s = zero) then block_bin
  else raise (CorruptedBlock ("Block "^Z.to_string block_sum^" is corrupted"))


(* Encrypt the channel `cin` and write
   the result on `cout` block by block.
   We use a "bit padding" scheme for the last block:
   a single '1' bit is added then as many '0' bits as required. *)
let encrypt_channel (pub_k:pub) cin cout =
  let block = Array.make pub_k.n 0 in
  let filled_size = ref 0 in

  let write_enc_block () =
    let enc_block = encrypt_block pub_k block in
    BatIO.write_string cout (Z.to_string enc_block) in

  try
    while true do
      let byte = BatIO.read_byte cin in
      for i = 0 to 7 do
        if !filled_size = pub_k.n then begin  (* we have a complete block *)
          write_enc_block ();
          filled_size := 0
        end;
        block.(!filled_size) <- byte lsr (7 - i) land 1;
        incr filled_size
      done
    done
  with BatIO.No_more_input ->
    (* if the last block is complete, we encrypt it and add a padding block *)
    if !filled_size = pub_k.n then begin
      write_enc_block ();
      block.(0) <- 1;
      for i = 1 to (pub_k.n - 1) do
        block.(i) <- 0
      done
    end
    (* else, we fill the current block with padding *)
    else begin
      block.(!filled_size) <- 1;
      for i = !filled_size + 1 to pub_k.n - 1 do
        block.(i) <- 0
      done
    end;
    write_enc_block ()

(* Decrypt the channel `cin` block by block and write
   the result on `cout` gradually.
   We use two buffers to delay the decryption to handle
   the padding in the last block.
   If a block is corrupted and/or cannot be decrypted,
   we compute the starting bit index for the next block
   so as to shift the channel to the next valid byte
   (only supported if the length of the key is > 8,
   which should always be the case).
   Call `f block` to decrypt a block of length `n`.
   This is a helper function which handles channels and
   padding, designed to be used with either a legitimate
   `f` decryption function or a cryptanalysis `f` function. *)
let decrypt_channel_aux f n cin cout flush no_color =
  let cur_byte_val = ref 0 in  (* [0, 255] *)
  let cur_bit_idx = ref 7 in   (* [0, 7] *)
  let buffer1 = ref Z.minus_one in
  let buffer2 = ref Z.minus_one in
  let nb_blocks = ref 0 in
  (* block index to start from after a corrupted block was encountered *)
  let after_corrupted_shift = ref 0 in

  let corrupted_text =
    if no_color then "(???)" else "\027[1;31m???\027[0m" in

  let write_dec_block block last =
    try
      incr nb_blocks;
      let dec_block = f block in  (* can raise CorruptedBlock *)
      (* there is no padding except for the last block *)
      let padding_start = ref (n - 1) in
      if last then begin  (* last block, find where the padding starts *)
        while dec_block.(!padding_start) = 0 do
          decr padding_start
        done;
        decr padding_start  (* for the '1' bit which was added *)
      end;

      (* if padding starts at 0 in the last block (i.e. full padding block),
         we ignore that block *)
      if not last || !padding_start <> 0 then begin
        for j = !after_corrupted_shift to !padding_start do
          if !cur_bit_idx = -1 then begin  (* we have a complete byte *)
            BatIO.write_byte cout !cur_byte_val;
            if flush then BatIO.flush cout;
            cur_byte_val := 0;
            cur_bit_idx := 7
          end;
          if dec_block.(j) = 1 then  (* add 2^cur_bit_idx *)
            cur_byte_val := !cur_byte_val lor (1 lsl !cur_bit_idx);
          decr cur_bit_idx
        done
      end;
      after_corrupted_shift := 0;
      if last then BatIO.write_byte cout !cur_byte_val

    with CorruptedBlock s ->
      if n < 8 then raise (CorruptedBlock s);
      BatIO.write_string cout corrupted_text;
      if flush then BatIO.flush cout;
      (* we find which part of the next block we should take out
         by shifting the channel to the next valid byte *)
      cur_byte_val := 0;
      cur_bit_idx := 7;
      let d = (!nb_blocks * n) mod 8 in
      after_corrupted_shift := if d > 0 then 8 - d else 0
  in

  try
    while true do
      buffer1 := Z.of_string (BatIO.read_string cin);
      if Z.(!buffer2 <> minus_one) then
        write_dec_block !buffer2 false;
      buffer2 := Z.of_string (BatIO.read_string cin);
      write_dec_block !buffer1 false;
      buffer1 := Z.minus_one
    done
  with
    | Invalid_argument _ ->  (* raised by Z.of_string *)
      raise (InvalidInput "The input is not an encrypted channel or is too damaged")
    | BatIO.No_more_input ->
      (* the last block is in buffer1 or buffer2 *)
      if Z.(!buffer1 <> minus_one) then
        write_dec_block !buffer1 true
      else if Z.(!buffer2 <> minus_one) then
        write_dec_block !buffer2 true

let decrypt_channel (priv_k:priv) cin cout no_color =
  decrypt_channel_aux (decrypt_block priv_k) priv_k.n cin cout false no_color


(* Cryptanalysis methods *)

let decrypt_channel_dynamic (pub_k:pub) cin cout flush no_color =
  try let key = Array.map Z.to_int pub_k.seq in
  let n = pub_k.n in

  let dec_dynamic block_sum =
    let block_sum = Z.to_int block_sum in
    let m = Array.make_matrix (n + 1) (block_sum + 1) false in
    for i = 0 to n do
      m.(i).(0) <- true
    done;
    for i = 1 to n do
      for j = 1 to block_sum do
        if j < key.(i - 1) then
          m.(i).(j) <- m.(i - 1).(j)
        else
          m.(i).(j) <- m.(i - 1).(j) || m.(i - 1).(j - key.(i-1))
      done
    done;
    let block_bin = Array.make n 0 in
    let i = ref n in
    let j = ref block_sum in
    while !i > 0 && !j > 0 do
      if not m.(!i - 1).(!j) then begin
        block_bin.(!i - 1) <- 1;
        j := !j - key.(!i - 1)
      end;
      decr i
    done;
    if !j = 0 then block_bin
    else raise (CorruptedBlock ("Block "^string_of_int block_sum^" is corrupted"))
  in

  decrypt_channel_aux dec_dynamic n cin cout flush no_color
  with Z.Overflow | Out_of_memory ->
    raise (KeySize "This method does not handle large keys - out of memory")


let decrypt_channel_tree (pub_k:pub) cin cout flush no_color =
  let n = pub_k.n in
  (* `key` is the sorted `pub_k.seq` and we store
     in `argsort` the indices that permitted to sort it *)
  let enum = Array.init n (fun i -> (i, pub_k.seq.(i))) in
  Array.fast_sort (fun (_, x) (_, y) -> Z.compare x y) enum;
  let argsort = Array.map fst enum in
  let key = Array.map snd enum in

  let r0 =  Array.fold_left Z.( + ) Z.zero key in

  let dec_tree sum =
    let block_bin = Array.make n 0 in
    let rec traversal s r k =
      let k' = argsort.(k) in
      if Z.(s + key.(k) = sum) then begin
        block_bin.(k') <- 1;
        raise Exit
      end
      else if k < n - 1 then begin
        let k_1 = k + 1 in
        (* visit right node if it can lead to a valid solution *)
        if Z.(s + r >= sum && s + key.(k) + key.(k_1) <= sum) then begin
          let tmp = block_bin.(k') in
          block_bin.(k') <- 1;
          traversal Z.(s + key.(k)) Z.(r - key.(k)) (k + 1);
          block_bin.(k') <- tmp
        end;
        (* visit left node if it can lead to a valid solution *)
        if Z.(s + r - key.(k) >= sum && s + key.(k_1) <= sum) then begin
          let tmp = block_bin.(k') in
          block_bin.(k') <- 0;
          traversal s Z.(r - key.(k)) (k + 1);
          block_bin.(k') <- tmp
        end
      end
    in
    if Z.(sum = zero) then block_bin
    else try
      traversal Z.zero r0 0;
      raise (CorruptedBlock ("Block "^Z.to_string sum^" is corrupted"))
    with Exit -> block_bin
  in

  decrypt_channel_aux dec_tree n cin cout flush no_color


let decrypt_channel_lll (pub_k:pub) cin cout flush no_color =
  let key = Array.map Z.to_bignum pub_k.seq in

  let dec_lll block =
    try
      LLL.solve key (Z.to_bignum block)
    with LLL.NoSolution ->
      raise (CorruptedBlock ("No solution found for block "^Z.to_string block))
  in

  decrypt_channel_aux dec_lll pub_k.n cin cout flush no_color
