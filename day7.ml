open! Core

let lines = In_channel.read_lines "day7.txt"

let categorize =
  let cd = Re.Pcre.regexp {|\$ cd (.+)|} in
  let ls = Re.Pcre.regexp {|\$ ls|} in
  let dir = Re.Pcre.regexp {|dir (.+)|} in
  let file = Re.Pcre.regexp {|(\d+) (.+)|} in
  fun s ->
    if Re.execp cd s then `Cd (Re.Pcre.extract ~rex:cd s).(1)
    else if Re.execp ls s then `Ls
    else if Re.execp dir s then `Dir (Re.Pcre.extract ~rex:dir s).(1)
    else
      let matches = Re.Pcre.extract ~rex:file s in
      `File (matches.(1) |> Int.of_string, matches.(2))

type dir = {
  name : string;
  children : tree Deque.t;
  parent : dir option; [@sexp.opaque]
}

and tree = Dir of dir | Leaf of (int * string) [@@deriving sexp]

let root' = { name = ""; children = Deque.create (); parent = None }

let root = Dir root'

let rec build_tree (cur_dir : dir) steps =
  match steps with
  | [] -> ()
  | step :: steps -> (
      match step with
      | `Cd ".." ->
          let cur_dir = cur_dir.parent |> Option.value_exn in
          build_tree cur_dir steps
      | `Cd dirname ->
          let cur_dir =
            cur_dir.children
            |> Deque.find_map ~f:(function
                 | Leaf _ -> None
                 | Dir dir ->
                     if String.( = ) dir.name dirname then Some dir else None)
            |> Option.value_exn
          in
          build_tree cur_dir steps
      | `Ls -> build_tree cur_dir steps
      | `Dir name ->
          let dir =
            { name; children = Deque.create (); parent = Some cur_dir }
          in
          Deque.enqueue_back cur_dir.children (Dir dir);
          build_tree cur_dir steps
      | `File file ->
          Deque.enqueue_back cur_dir.children (Leaf file);
          build_tree cur_dir steps)

let steps = List.tl_exn lines |> List.map ~f:categorize

let () = build_tree root' steps

let rec accum ~update_acc acc tree =
  match tree with
  | Leaf (size, _) -> (acc, size)
  | Dir { children; _ } ->
      let acc, size =
        Deque.fold children ~init:(acc, 0) ~f:(fun (acc, size) node ->
            let acc, size' = accum ~update_acc acc node in
            (acc, size + size'))
      in
      let acc = update_acc acc size in
      (acc, size)

let part1 () =
  let update_acc acc size = if size <= 100000 then size + acc else acc in
  let acc, _ = accum ~update_acc 0 root in
  printf "%d\n" acc

let%expect_test _ =
  part1 ();
  [%expect {| 1077191 |}]

let part2 () =
  let update_acc acc _ = acc in
  let _acc, used_space = accum ~update_acc 0 root in
  let needed_space = used_space - 40000000 in
  let update_acc acc size =
    if size <= needed_space then acc
    else
      match acc with
      | Some size' when size' < size -> Some size'
      | _ -> Some size
  in
  let acc = accum ~update_acc None root |> fst |> Option.value_exn in
  printf "%d\n" acc

let%expect_test _ =
  part2 ();
  [%expect {| 5649896 |}]
