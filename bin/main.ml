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

let dirname (fn : filename) : dirname =
    implode (List.tl (explode fn))

let rec replace_pref : string list -> string list -> string list -> string list =
    fun xs ys zs ->
    match xs, zs with
    | x::xx, z::zz ->
      if x = z
      then replace_pref xx ys zz
      else failwith "bad replace_pref"
    | [], zz -> ys @ zz
    | _::_, [] -> failwith "replace_pref: bad call"

(* Moves a file from the `old` directory into `new`, retaining
   all intermediate directories. `fn` must already be in `old`
   (i.e. old should be a prefix of fn). *)
let copy_graft (old : dirname) (nu : dirname) (fn : filename) : unit =
    let old_c = explode old in
    let nu_c = explode nu in
    let tgt = replace_pref old_c nu_c (explode fn) |> implode in
    Printf.printf "pretend I'm copying %s to %s\n" fn tgt;
    ()


let () =
    print_endline ("Running in " ^ getcwd());
    let base = "FStar" in
    let l = find_smt2 base in
    Printf.printf "Found %s SMT2 files in %s\n" (string_of_int (List.length l)) base;
    List.iter (copy_graft "FStar" "all-queries") l;
    ()
