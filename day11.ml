open! Core

let example =
  {|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1|}
;;

let input = In_channel.read_all "day11.txt"

let rex =
  Re.Pcre.regexp
    ~flags:[ `MULTILINE ]
    {|Monkey (\d):
 *Starting items: (.+)
 *Operation: new = old (.+)
 *Test: divisible by (\d+)
 *If true: throw to monkey (\d+)
 *If false: throw to monkey (\d+)|}
;;

type monkey =
  { items : int list
  ; operation : int -> int
  ; throw : int -> int
  ; num_inspections : int
  }

let parse input =
  let monkeys =
    input
    |> Re.split (Re.Pcre.regexp ~flags:[ `MULTILINE ] "\n\n")
    |> List.mapi ~f:(fun _i monkey -> Array.slice (Re.Pcre.extract ~rex monkey) 1 0)
    |> List.map ~f:(fun x -> x)
    |> List.map ~f:(fun m ->
         let items =
           String.split ~on:',' m.(1)
           |> List.map ~f:(fun x -> Int.of_string (String.strip x))
         in
         let operation =
           let op, num = String.lsplit2_exn ~on:' ' m.(2) in
           fun x ->
             let num = if String.(num = "old") then x else Int.of_string num in
             match op with
             | "+" -> x + num
             | "*" -> x * num
             | _ -> failwith "mul and add"
         in
         let divisor, throw =
           let divisor = Int.of_string m.(3) in
           let t = Int.of_string m.(4) in
           let f = Int.of_string m.(5) in
           divisor, fun x -> if x % divisor = 0 then t else f
         in
         { items; operation; throw; num_inspections = 0 }, divisor)
  in
  List.mapi monkeys ~f:Tuple2.create |> Int.Map.of_alist_exn
;;

let foo monkeys rounds op =
  rounds
  |> List.init ~f:Fn.id
  |> List.fold ~init:monkeys ~f:(fun monkeys _i ->
       monkeys
       |> Map.length
       |> List.init ~f:Fn.id
       |> List.fold ~init:monkeys ~f:(fun monkeys i ->
            let monkey = Map.find_exn monkeys i in
            let monkey' =
              { monkey with
                items = []
              ; num_inspections = monkey.num_inspections + List.length monkey.items
              }
            in
            let monkeys = Map.set monkeys ~key:i ~data:monkey' in
            List.fold monkey.items ~init:monkeys ~f:(fun monkeys item ->
              let item = op (monkey.operation item) in
              let to_ = monkey.throw item in
              Map.update monkeys to_ ~f:(fun monkey ->
                Option.map monkey ~f:(fun monkey ->
                  { monkey with items = monkey.items @ [ item ] })
                |> Option.value_exn))))
  |> Map.to_alist
  |> List.map ~f:snd
  |> List.sort ~compare:(fun x y -> y.num_inspections - x.num_inspections)
  |> (Fn.flip List.take) 2
  |> List.map ~f:(fun monkey -> monkey.num_inspections)
  |> List.reduce_exn ~f:( * )
  |> printf "%d"
;;

let part1 input =
  let monkeys = parse input |> Map.map ~f:fst in
  foo monkeys 20 (fun i -> i / 3)
;;

let part2 input =
  let monkeys = parse input in
  let mod_ = Map.map monkeys ~f:snd |> Map.data |> List.reduce_exn ~f:( * ) in
  foo (Map.map monkeys ~f:fst) 10000 (fun i -> i % mod_)
;;

let%expect_test _ =
  part1 example;
  [%expect {| 10605 |}];
  part1 input;
  [%expect {| 61503 |}];
  part2 example;
  [%expect {| 2713310158 |}];
  part2 input;
  [%expect {| 14081365540 |}]
;;
