open! Core

let lines = In_channel.read_lines "day6.txt"

let input =
  match lines with [ line ] -> line |> String.to_list | _ -> failwith "no"

let do_ n =
  List.init n ~f:(fun i -> List.slice input i (List.length input - n + i))
  |> List.transpose_exn
  |> List.map ~f:Char.Set.of_list
  |> List.findi ~f:(fun _ s -> Set.length s = n)
  |> Option.value_exn |> fst |> ( + ) n |> printf "%d"

let part1 () = do_ 4

let%expect_test _ =
  part1 ();
  [%expect {| 1850 |}]

let part2 () = do_ 14

let%expect_test _ =
  part2 ();
  [%expect {| 2823 |}]
