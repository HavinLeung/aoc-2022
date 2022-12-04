open! Core

let lines = In_channel.read_lines "day4.txt"

let parsed =
  let parse_range range =
    match String.split ~on:'-' range with
    | [ l; r ] -> (Int.of_string l, Int.of_string r)
    | _ -> failwith "no"
  in
  List.map lines ~f:(fun line ->
      match String.split ~on:',' line with
      | [ a; b ] -> (parse_range a, parse_range b)
      | s -> raise_s [%message (s : string list)])

let contains (l1, r1) (l2, r2) = (l1 <= l2 && r1 >= r2) || (l2 <= l1 && r2 >= r1)

let part1 () =
  List.map parsed ~f:(fun (range1, range2) ->
      if contains range1 range2 then 1 else 0)
  |> List.reduce_exn ~f:( + ) |> printf "%d"

let%expect_test _ =
  part1 ();
  [%expect {|475|}]

let overlaps (l1, r1) (l2, r2) = (l1 >= l2 && l1 <= r2) || (l2 >= l1 && l2 <= r1)

let part2 () =
  List.map parsed ~f:(fun (range1, range2) ->
      if overlaps range1 range2 then 1 else 0)
  |> List.reduce_exn ~f:( + ) |> printf "%d"

let%expect_test _ =
  part2 ();
  [%expect {|825|}]
