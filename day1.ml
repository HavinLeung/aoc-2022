open! Core

let lines = In_channel.read_lines "day1.txt"

let by_elf =
  let elves, elf =
    List.fold lines ~init:([], 0) ~f:(fun (elves, cur_elf) line ->
        match line with
        | "" -> (cur_elf :: elves, 0)
        | calories ->
            let calories = Int.of_string calories in
            (elves, cur_elf + calories))
  in
  elf :: elves |> List.rev

let part1 () =
  List.max_elt by_elf ~compare:[%compare: int]
  |> Option.value_exn |> printf "%d"

let%expect_test _ =
  part1 ();
  [%expect {| 67027 |}]

let part2 () =
  let sorted = List.sort by_elf ~compare:[%compare: int] |> List.rev in
  match sorted with
  | a :: b :: c :: _ -> printf "%d %d %d" a b c
  | _ -> failwith "no"

let%expect_test _ =
  part2 ();
  [%expect {| 67027 65333 64931 |}]
