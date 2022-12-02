open! Core

let lines = In_channel.read_lines "day2.txt"

module RPS = struct
  type t = Rock | Paper | Scissors

  let of_string s =
    match s with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | _ -> failwith "parse error"

  let points_from_outcome t1 t2 =
    match (t1, t2) with
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
    | Paper, Rock | Rock, Scissors | Scissors, Paper -> 6
    | _ -> 0

  let points_from_shape = function Rock -> 1 | Paper -> 2 | Scissors -> 3
end

let parsed =
  List.map lines ~f:(fun line ->
      match String.split ~on:' ' line with
      | [ opponent; you ] -> (RPS.of_string opponent, RPS.of_string you)
      | _ -> failwith "no")

let part1 () =
  List.fold parsed ~init:0 ~f:(fun acc (opp, you) ->
      acc + RPS.points_from_outcome you opp + RPS.points_from_shape you)
  |> printf "%d"

let%expect_test _ =
  part1 ();
  [%expect {|11666|}]

module RPS_2 = struct
  type t = Rock | Paper | Scissors

  type outcome = Lose | Draw | Win

  let of_string s =
    match s with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "parse error"

  let outcome_of_string s =
    match s with
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "parse error"

  let points_from_outcome (outcome : outcome) =
    match outcome with Lose -> 0 | Draw -> 3 | Win -> 6

  let shape_from_outcome_and_opp opp outcome =
    match (opp, outcome) with
    | opp, Draw -> opp
    | Rock, Win -> Paper
    | Paper, Win -> Scissors
    | Scissors, Win -> Rock
    | Rock, Lose -> Scissors
    | Paper, Lose -> Rock
    | Scissors, Lose -> Paper

  let points_from_shape = function Rock -> 1 | Paper -> 2 | Scissors -> 3
end

let parsed =
  List.map lines ~f:(fun line ->
      match String.split ~on:' ' line with
      | [ opponent; you ] ->
          (RPS_2.of_string opponent, RPS_2.outcome_of_string you)
      | _ -> failwith "no")

let part2 () =
  List.fold parsed ~init:0 ~f:(fun acc (opp, outcome) ->
      let your_shape = RPS_2.shape_from_outcome_and_opp opp outcome in
      acc
      + RPS_2.points_from_outcome outcome
      + RPS_2.points_from_shape your_shape)
  |> printf "%d"

let%expect_test _ =
  part2 ();
  [%expect {|12767|}]
