open! Core

let example = {|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|} |> String.split_lines

let input = In_channel.read_lines "day9.txt"

let parse lines =
  List.map lines ~f:(String.lsplit2_exn ~on:' ')
  |> List.map ~f:(Tuple2.map_snd ~f:Int.of_string)
  |> List.map
       ~f:
         (Tuple2.map_fst ~f:(fun x ->
              Sexp.Atom x |> [%of_sexp: [ `R | `L | `U | `D ]]))

module Coord = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let zero = (0, 0)

  let sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

  let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

  let div (x, y) n = (x / n, y / n)

  let follow knots dir =
    let follow h t =
      let touching_origin = function
        | (-1 | 0 | 1), (-1 | 0 | 1) -> true
        | _ -> false
      in
      match sub h t with
      | (0, (2 | -2) | (2 | -2), 0) as dir -> add t (div dir 2)
      | x when touching_origin x -> t
      | _ ->
          let diag_moves =
            let x = [ -1; 1 ] in
            List.cartesian_product x x
          in
          List.map diag_moves ~f:(add t)
          |> List.find_exn ~f:(Fn.compose touching_origin (sub h))
    in
    let dir_to_t = function
      | `R -> (1, 0)
      | `L -> (-1, 0)
      | `U -> (0, 1)
      | `D -> (0, -1)
    in
    let rec move_knots head knots acc : t list =
      match knots with
      | [] -> head :: acc |> List.rev
      | knot :: knots ->
          let knot = follow head knot in
          move_knots knot knots (head :: acc)
    in
    let dir = dir_to_t dir in
    match knots with
    | hd :: tl -> move_knots (add hd dir) tl []
    | _ -> failwith "empty list"
end

let do_ lines n =
  let open Coord in
  let input = parse lines in
  let visited = Set.singleton zero in
  let knots = List.init n ~f:(Fn.const zero) in
  let knots, visited =
    List.fold ~init:(knots, visited) input
      ~f:(fun (knots, visited) (dir, num) ->
        List.init num ~f:Fn.id
        |> List.fold ~init:(knots, visited) ~f:(fun (knots, visited) _ ->
               let knots = follow knots dir in
               let visited = Set.add visited (List.last_exn knots) in
               (knots, visited)))
  in
  print_s [%message (knots : t list) (Set.length visited : int)]

let do_example = do_ example

let do_ = do_ input

let part1 () =
  do_example 2;
  do_ 2

let%expect_test _ =
  part1 ();
  [%expect
    {|
    ((knots ((2 2) (1 2))) ("Set.length visited" 13))
    ((knots ((121 -89) (121 -88))) ("Set.length visited" 6354)) |}]

let part2 () =
  do_example 10;
  do_ 10

let%expect_test _ =
  part2 ();
  [%expect
    {|
  ((knots ((2 2) (1 2) (2 2) (3 2) (2 2) (1 1) (0 0) (0 0) (0 0) (0 0)))
   ("Set.length visited" 1))
  ((knots
    ((121 -89) (121 -88) (121 -87) (121 -86) (121 -85) (121 -84) (121 -83)
     (121 -83) (121 -84) (121 -85)))
   ("Set.length visited" 2651)) |}]
