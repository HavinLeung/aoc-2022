open! Core

let lines = In_channel.read_lines "day3.txt"

let priority (c : Char.t) =
  match c with
  | 'a' .. 'z' -> Char.to_int c + 1 - Char.to_int 'a'
  | 'A' .. 'Z' -> Char.to_int c + 27 - Char.to_int 'A'
  | _ -> failwith "invalid char"

let part1 () =
  List.map lines ~f:(fun line ->
      let chars = String.to_list line in
      List.split_n chars (List.length chars / 2))
  |> List.map ~f:(fun (l1, l2) ->
         let l1 = Char.Set.of_list l1 in
         let l2 = Char.Set.of_list l2 in
         Set.inter l1 l2 |> Set.elements |> List.hd_exn)
  |> List.map ~f:priority |> List.reduce_exn ~f:( + ) |> printf "%d"

let%expect_test _ =
  part1 ();
  [%expect {|8493|}]

let part2 () =
  let to_3_tuple l =
    match l with [ a; b; c ] -> (a, b, c) | _ -> failwith "not a len 3 list"
  in
  List.map lines ~f:String.to_list
  |> List.map ~f:Char.Set.of_list
  |> List.fold ~init:([], []) ~f:(fun (acc, cur) c ->
         if List.length cur = 3 then (cur :: acc, [ c ]) else (acc, c :: cur))
  |> (fun (a, b) -> b :: a)
  |> List.map ~f:to_3_tuple
  |> List.map ~f:(fun (a, b, c) ->
         Set.inter a b |> Set.inter c |> Set.elements |> List.hd_exn)
  |> List.map ~f:priority |> List.reduce_exn ~f:( + ) |> printf "%d"

let%expect_test _ =
  part2 ();
  [%expect {|2552|}]
