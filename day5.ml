open! Core

let lines = In_channel.read_lines "day5.txt"

let stack, moves =
  let stack, moves = List.split_while lines ~f:(Fn.non String.is_empty) in
  (List.drop_last_exn stack, List.tl_exn moves)

let moves =
  let rex = Re.Pcre.regexp {|move (\d+) from (\d+) to (\d+)|} in
  List.map moves ~f:(fun s -> Re.Pcre.extract ~rex s)
  |> List.map ~f:(fun arr -> (arr.(1), arr.(2), arr.(3)))
  |> List.map ~f:(Tuple3.map ~f:Int.of_string)

let stack =
  List.map stack ~f:String.to_list
  |> List.transpose_exn
  |> List.filteri ~f:(fun i _ -> (i - 1) % 4 = 0)
  |> List.map ~f:(List.drop_while ~f:Char.is_whitespace)

let do_ append =
  let stack = List.to_array stack in
  List.iter moves ~f:(fun (num, from, to_) ->
      let from, to_ = (from - 1, to_ - 1) in
      let top, bot = List.split_n stack.(from) num in
      stack.(from) <- bot;
      stack.(to_) <- append top stack.(to_));
  let stack =
    Array.map stack ~f:List.hd_exn |> Array.to_list |> String.of_char_list
  in
  print_s [%message (stack : string)]

let part1 () = do_ List.rev_append

let%expect_test _ =
  part1 ();
  [%expect {| (stack QPJPLMNNR) |}]

let part2 () = do_ List.append

let%expect_test _ =
  part2 ();
  [%expect {| (stack BQDNWJPVJ) |}]
