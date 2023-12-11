(* This is a quick-and-dirty wrapper over Domainslib.Task
   that makes teardown_pool wait until all tasks have finished,
   instead of being unspecified in that case. *)

module T = Domainslib.Task
module A = Atomic
module S = Semaphore.Binary
open Either

type 'a task = 'a T.task

type !'a promise = 'a T.promise

type pool = {
    p   : T.pool;
    ctr : int Atomic.t;
    sem : S.t;
}

let setup_pool ?name ~num_domains () =
    let p = T.setup_pool ?name ~num_domains () in
    let ctr = Atomic.make 0 in
    let sem = S.make false in
    { p; ctr; sem }

let teardown_pool p =
    S.acquire p.sem;
    T.teardown_pool p.p

let lookup_pool _name =
    (* IOU: we can just keep a hashtable here and not pass a name to setup_pool. *)
    None

let get_num_domains p = T.get_num_domains p.p

let wrap_task p (t : 'a task) : 'a task = fun () ->
    let r =
        match t () with
        | exception ex -> Left ex
        | v -> Right v
    in
    let c = A.fetch_and_add p.ctr (-1) in
    if c = 1 then (* c == old value *)
        S.release p.sem;
    match r with
    | Left ex -> raise ex (* this will mess with stack traces, but at least won't deadlock *)
    | Right v -> v

let run p t =
    ignore (A.fetch_and_add p.ctr 1);
    T.run p.p (wrap_task p t)

let async p t =
    ignore (A.fetch_and_add p.ctr 1);
    T.async p.p (wrap_task p t)

let await p h =
    T.await p.p h

let parallel_for ?chunk_size ~start ~finish ~body pool =
    T.parallel_for ?chunk_size ~start ~finish ~body pool.p

let parallel_for_reduce ?chunk_size ~start ~finish ~body pool f z =
    T.parallel_for_reduce ?chunk_size ~start ~finish ~body pool.p f z

let parallel_scan pool f a =
    T.parallel_scan pool.p f a

let parallel_find ?chunk_size ~start ~finish ~body pool =
    T.parallel_find ?chunk_size ~start ~finish ~body pool.p
