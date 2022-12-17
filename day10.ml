open! Core

let example =
  {|addx 15,addx -11,addx 6,addx -3,addx 5,addx -1,addx -8,addx 13,addx 4,noop,addx -1,addx 5,addx -1,addx 5,addx -1,addx 5,addx -1,addx 5,addx -1,addx -35,addx 1,addx 24,addx -19,addx 1,addx 16,addx -11,noop,noop,addx 21,addx -15,noop,noop,addx -3,addx 9,addx 1,addx -3,addx 8,addx 1,addx 5,noop,noop,noop,noop,noop,addx -36,noop,addx 1,addx 7,noop,noop,noop,addx 2,addx 6,noop,noop,noop,noop,noop,addx 1,noop,noop,addx 7,addx 1,noop,addx -13,addx 13,addx 7,noop,addx 1,addx -33,noop,noop,noop,addx 2,noop,noop,noop,addx 8,noop,addx -1,addx 2,addx 1,noop,addx 17,addx -9,addx 1,addx 1,addx -3,addx 11,noop,noop,addx 1,noop,addx 1,noop,noop,addx -13,addx -19,addx 1,addx 3,addx 26,addx -30,addx 12,addx -1,addx 3,addx 1,noop,noop,noop,addx -9,addx 18,addx 1,addx 2,noop,noop,addx 9,noop,noop,noop,addx -1,addx 2,addx -37,addx 1,addx 3,noop,addx 15,addx -21,addx 22,addx -6,addx 1,noop,addx 2,addx 1,noop,addx -10,noop,noop,addx 20,addx 1,addx 2,addx 2,addx -6,addx -11,noop,noop,noop|}
  |> String.split ~on:','
;;

let input = In_channel.read_lines "day10.txt"

let parse =
  List.map ~f:(fun line ->
      match String.split ~on:' ' line with
      | [ "addx"; n ] -> `Addx (Int.of_string n)
      | [ "noop" ] -> `Noop
      | _ -> failwith "no")
;;

let cycles_of = function
  | `Addx (_ : int) -> 2
  | `Noop -> 1
;;

type part1 =
  { x : int
  ; cycle : int
  }

let should_record cycle = (cycle + 20) % 40 = 0

let part1 input =
  input
  |> parse
  |> List.fold
       ~init:({ x = 1; cycle = 0 }, 0)
       ~f:(fun ({ x; cycle }, sum) instr ->
         let cycle = cycles_of instr + cycle in
         let x', add =
           match instr with
           | `Noop -> x, if should_record cycle then x else 0
           | `Addx n ->
             x + n, if should_record cycle || should_record (cycle - 1) then x else 0
         in
         { x = x'; cycle }, sum + (add * (cycle - (cycle % 2))))
  |> snd
  |> printf "%d\n"
;;

let%expect_test "pt1" =
  part1 example;
  part1 input;
  [%expect {|
    13140
    14860 |}]
;;

let draw cycle x =
  if cycle % 40 = 1 then printf "\n";
  let cycle = cycle % 40 in
  (if List.mem [ x; x + 1; x + 2 ] cycle ~equal:( = ) then '#' else '.') |> printf "%c"
;;

let part2 input =
  input
  |> parse
  |> List.fold ~init:(0, 1) ~f:(fun (cycle, x) instr ->
         match instr with
         | `Noop ->
           draw (cycle + 1) x;
           cycle + 1, x
         | `Addx n ->
           draw (cycle + 1) x;
           draw (cycle + 2) x;
           cycle + 2, x + n)
  |> Fn.ignore
;;

let%expect_test "pt1" =
  part2 example;
  [%expect
    {|
    ##..##..##..##..##..##..##..##..##..##..
    ###...###...###...###...###...###...###.
    ####....####....####....####....####....
    #####.....#####.....#####.....#####.....
    ######......######......######......###.
    #######.......#######.......#######..... |}];
  part2 input;
  [%expect
    {|
    ###...##..####.####.#..#.#..#.###..#..##
    #..#.#..#....#.#....#..#.#..#.#..#.#.#.#
    #..#.#......#..###..####.#..#.#..#.##...
    ###..#.##..#...#....#..#.#..#.###..#.#.#
    #.#..#..#.#....#....#..#.#..#.#.#..#.#.#
    #..#..###.####.####.#..#..##..#..#.#..#. |}]
;;
