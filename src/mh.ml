(* Error handling is mainly done by Crypto by raising exceptions
   that we don't always catch here. *)

let rec read_line_nonempty msg =
  let s = ref "" in
  while !s = "" do
    print_string msg;
    s := read_line ()
  done;
  !s

let genkey size name dir d f =
  let d = match d with None -> 20 | Some x -> x in
  let f = match f with None -> 2 | Some x -> x in

  let name = match name with
    | Some s -> s
    | None -> read_line_nonempty "Name: "
  in
  let dir = match dir with
    | Some s -> s
    | None -> print_string "Directory to write the keys in [default .]: ";
              let d = read_line () in if d <> "" then d
              else Filename.current_dir_name
  in
  let pubout = Filename.concat dir (name^".pub") in
  let privout = Filename.concat dir name in

  let pub_k, priv_k = Crypto.create_keys size d f in
  Crypto.save_keys (pub_k, pubout) (priv_k, privout);
  print_endline "The keys were successfully created!"

let showkey path =
  try
    let pub_k = Crypto.load_pub_key path in
    print_endline "Public key";
    Crypto.fprint_pub stdout pub_k
  with Crypto.InvalidKey _ ->
    let priv_k = Crypto.load_priv_key path in
    print_endline "Private key";
    Crypto.fprint_priv stdout priv_k

(* Open two BatIO channels based on options `cin` and `cout`.
   By default, use stdin and stdout. *)
let open_channels_opt cin cout =
  try
    let cin = match cin with
      None -> stdin | Some file -> open_in_bin file in
    let cout = match cout with
      None -> stdout | Some file -> open_out_bin file in
    (BatIO.input_channel ~cleanup:true cin,
     BatIO.output_channel ~cleanup:true cout)
  with Sys_error _ ->
    failwith "Cannot read or write in the given channels"

let close_channels cin cout =
  BatIO.close_in cin;
  BatIO.close_out cout

let encrypt pub_k cin cout =
  let pub_k = Crypto.load_pub_key pub_k in
  let (cin, cout) = open_channels_opt cin cout in
  Crypto.encrypt_channel pub_k cin cout;
  close_channels cin cout

let decrypt priv_k cin cout no_color =
  let priv_k = Crypto.load_priv_key priv_k in
  let (cin, cout) = open_channels_opt cin cout in
  Crypto.decrypt_channel priv_k cin cout no_color;
  close_channels cin cout

let atk m pub_k cin cout flush no_color =
  let pub_k = Crypto.load_pub_key pub_k in
  let (cin, cout) = open_channels_opt cin cout in
  let _ = match m with
  | "dynamic" ->
    Crypto.decrypt_channel_dynamic pub_k cin cout flush no_color;
  | "tree" ->
    Crypto.decrypt_channel_tree pub_k cin cout flush no_color;
  | "lll" ->
    Crypto.decrypt_channel_lll pub_k cin cout flush no_color;
  | s -> close_channels cin cout; failwith (s^": invalid method for atk")
  in close_channels cin cout


(* Commands *)

open Cmdliner

let copts_sect = "COMMON OPTIONS"
let help_secs = [
 `S copts_sect;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help` for help on a single command."; `Noblank]

let genkey_cmd =
  let size =
    let doc = "The length of the key." in
    Arg.(required & pos 0 (some int) None & info [] ~docv:"SIZE" ~doc)
  in
  let key_name =
    let doc = "Name of the key (will produce $(i,NAME) and $(i,NAME).pub)." in
    Arg.(value & pos 1 (some string) None & info [] ~docv:"NAME" ~doc)
  in
  let dir =
    let doc = "Write the keys in the directory $(i,DIR)." in
    Arg.(value & pos 2 (some dir) None & info [] ~docv:"DIR" ~doc)
  in
  let d =
    let doc = "See DESCRIPTION." in
    Arg.(value & opt (some int) None & info ["d"] ~docv:"D" ~doc)
  in
  let f =
    let doc = "See DESCRIPTION." in
    Arg.(value & opt (some int) None & info ["f"] ~docv:"F" ~doc)
  in
  let doc = "key generator" in
  let man =
    [`S "DESCRIPTION";
     `P "Generate a pair of public and private keys of a given $(i,SIZE).
         When not given, ask on standard input the name and the directory
         for the keys.";
     `P "$(i,D) and $(i,F) are two int constants used to build the
         knapsack and affecting its density. For a key of a given length,
         you may want a larger value for $(i,D*F) to protect from a dynamic attack,
         and a lower value for $(i,D*F) to protect from LLL (high density)."] @ help_secs
  in
  Term.(const genkey $ size $ key_name $ dir $ d $ f),
  Term.info "genkey" ~doc ~sdocs:copts_sect ~man

let showkey_cmd =
  let path =
    let doc = "The key to display, either public or private." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"KEY_FILE" ~doc)
  in
  let doc = "print human-readable keys" in
  let man =
    [`S "DESCRIPTION";
     `P "Write a public or private $(i,KEY_FILE) on standard output in a
         human-redeable format."] @ help_secs
  in
  Term.(const showkey $ path),
  Term.info "showkey" ~doc ~sdocs:copts_sect ~man

let encrypt_cmd =
  let pub_k =
    let doc = "The public key to use." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"PUB_KEY_FILE" ~doc)
  in
  let cin =
    let doc = "Use $(i,FILE) as input (optional, default stdin)." in
    Arg.(value & pos 1 (some file) None & info [] ~docv:"FILE" ~doc)
  in
  let cout =
    let doc = "Use $(i,FILE) as output (overwrite or create it)." in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)
  in
  let doc = "channel encryption" in
  let man =
    [`S "DESCRIPTION";
     `P "Encrypt a channel with the public key $(i,PUB_KEY_FILE).
         By default, read on standard input and write the encrypted channel
         on standard output."] @ help_secs
  in
  Term.(const encrypt $ pub_k $ cin $ cout),
  Term.info "encrypt" ~doc ~sdocs:copts_sect ~man

let decrypt_cmd =
  let priv_k =
    let doc = "The private key to use." in
    Arg.(required & pos 0 (some file) None & info [] ~docv:"PRIV_KEY_FILE" ~doc)
  in
  let cin =
    let doc = "Use $(i,FILE) as input (optional, default stdin)." in
    Arg.(value & pos 1 (some file) None & info [] ~docv:"FILE" ~doc)
  in
  let cout =
    let doc = "Use $(i,FILE) as output (overwrite or create it)." in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)
  in
  let no_color =
    let doc = "Don't use color for corrupted blocks." in
    Arg.(value & flag & info ["nc"; "no-color"] ~doc)
  in
  let doc = "channel decryption" in
  let man =
    [`S "DESCRIPTION";
     `P "Decrypt a channel with the private key $(i,PRIV_KEY_FILE).
         By default, read on standard input and write the decrypted channel
         on standard output."] @ help_secs
  in
  Term.(const decrypt $ priv_k $ cin $ cout $ no_color),
  Term.info "decrypt" ~doc ~sdocs:copts_sect ~man

let atk_cmd =
  let m =
    let doc = "The method to use: $(i,dynamic), $(i,tree) or $(i,lll)." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"METHOD" ~doc)
  in
  let pub_k =
    let doc = "The public key to use." in
    Arg.(required & pos 1 (some file) None & info [] ~docv:"PUB_KEY_FILE" ~doc)
  in
  let cin =
    let doc = "Use $(i,FILE) as input (optional, default stdin)." in
    Arg.(value & pos 2 (some file) None & info [] ~docv:"FILE" ~doc)
  in
  let cout =
    let doc = "Use $(i,FILE) as output (overwrite or create it)." in
    Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)
  in
  let flush =
    let doc = "Flush the output channel after each decrypted block is written." in
    Arg.(value & flag & info ["f"; "flush"] ~doc)
  in
  let no_color =
    let doc = "Turn off color for corrupted blocks." in
    Arg.(value & flag & info ["nc"; "no-color"] ~doc)
  in
  let doc = "channel decryption by cryptanalysis" in
  let man =
    [`S "DESCRIPTION";
     `P "Try to decrypt a channel with the public key $(i,PUB_KEY_FILE) and the
         cryptanalysis method $(i,METHOD).
         By default, read on standard input and write the decrypted channel
         on standard output."] @ help_secs
  in
  Term.(const atk $ m $ pub_k $ cin $ cout $ flush $ no_color),
  Term.info "atk" ~doc ~sdocs:copts_sect ~man

let default_cmd =
  let doc = "an implementation of the Merkle-Hellman cryptosystem
             with cryptanalysis tools" in
  let man = help_secs in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "mh" ~version:"1.0.0" ~sdocs:copts_sect ~doc ~man

let cmds = [genkey_cmd; showkey_cmd; encrypt_cmd; decrypt_cmd; atk_cmd]

let () =
  match Term.eval_choice default_cmd cmds ~catch:false with
  | `Error _ -> exit 1
  | _        -> exit 0
