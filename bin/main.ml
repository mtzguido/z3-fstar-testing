open Unix

let tag =
    (* Unix.system "date +'%Y-%m-%d-%H:%M:%S'" *)
    "fresh"

let ends_with (suf:string) (s:string) : bool =
    let l = String.length suf in
    String.length s >= l &&
    String.sub s (String.length s - l) l = suf

type filename = string
type dirname  = string

let find_files
    (pred : filename -> bool)
    (from : dirname) : filename list
=
  let acc : filename list ref = ref [] in
  let rec go (rel:string) (dh : dir_handle) =
    match readdir dh with
    | exception End_of_file -> ()
    | fn when fn = "." || fn = ".." -> (* haha *)
      go rel dh
    | fn ->
      if pred (rel ^ "/" ^ fn) then (
        acc := (rel ^ "/" ^ fn) :: !acc;
        go rel dh
      ) else (
        find_files_in (rel ^ "/" ^ fn);
        go rel dh
      )
  and find_files_in (from:string) =
      match opendir from with
      | exception Unix_error(Unix.ENOTDIR, _, _) -> ()
      | dh -> go from dh; closedir dh
  in
  find_files_in from;
  !acc

let find_smt2 (from : dirname) : filename list =
    find_files (ends_with ".smt2") from

let explode : filename -> string list =
  String.split_on_char '/'
let implode : string list -> filename =
  String.concat "/"

let init s = List.rev (List.tl (List.rev s))

let dirname (fn : filename) : dirname =
    implode (init (explode fn))

let rec replace_pref : string list -> string list -> string list -> string list =
    fun xs ys zs ->
    match xs, zs with
    | x::xx, z::zz ->
      if x = z
      then replace_pref xx ys zz
      else failwith "bad replace_pref"
    | [], zz -> ys @ zz
    | _::_, [] -> failwith "replace_pref: bad call"

let rec mkdir_p (d : dirname) : unit =
    try
      Unix.mkdir d 0o755;
    print_string "ok?\n"
    with
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
        mkdir_p (dirname d);
        Unix.mkdir d 0o755
      | Unix.Unix_error (Unix.EEXIST, _, _) ->
        ()

let copy_file (from : filename) (to_ : filename) : unit =
    let c_in = open_in from in
    mkdir_p (dirname to_);
    let c_out = open_out to_ in
    let buf = Bytes.make 4096 '0' in
    let rec go () =
      (* let line = input_line c_in in *)
      (* output_string c_out (line ^ "\n"); *)
      let n = input c_in buf 0 4096 in
      if n > 0 then (
          output c_out buf 0 n;
          go ()
      )
    in
    (try
      go()
    with | End_of_file -> ());
    close_in c_in;
    close_out c_out;
    Printf.printf "OK copy %s -> %s\n" from to_;
    ()

(* Moves a file from the `old` directory into `new`, retaining
   all intermediate directories. `fn` must already be in `old`
   (i.e. old should be a prefix of fn). *)
let copy_graft (old : dirname) (nu : dirname) (fn : filename) : string =
    let old_c = explode old in
    let nu_c = explode nu in
    let tgt = replace_pref old_c nu_c (explode fn) |> implode in
    copy_file fn tgt;
    tgt

let run1 (fn : filename) : unit =
    let _ = Unix.system (Printf.sprintf "z3-4.8.5 %s > %s.out" fn fn) in
    ()

let () =
    print_endline ("Running in " ^ getcwd());
    if Array.length Sys.argv < 2 then (
        Printf.printf "Usage: %s <dir>\n" (Sys.argv.(0));
        exit 1
    );
    let base = Sys.argv.(1) in

    let l = find_smt2 base in
    Printf.printf "Found %s SMT2 files in %s\n" (string_of_int (List.length l)) base;
    Printf.printf "Bringing them to all-queries\n";
    let l' = List.map (copy_graft base "all-queries") l in
    Printf.printf "Running z3-4.8.5 and saving output\n";

    let p = Task.setup_pool ~num_domains:32 () in
    List.iter (fun fn -> ignore (Task.async p (fun () -> run1 fn))) l';
    Task.teardown_pool p;
    ()
