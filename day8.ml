open! Core

let lines = In_channel.read_lines "day8.txt"

let example = {|30373
25512
65332
33549
35390|} |> String.split_lines

let input lines =
  List.map lines ~f:String.to_list
  |> List.map ~f:(List.map ~f:(Fn.compose Int.of_string String.of_char))

let part1 lines =
  let input = input lines in
  let m = List.length input in
  let n = List.length (List.hd_exn input) in
  let seen = Array.init m ~f:(fun _ -> Array.init n ~f:(fun _ -> false)) in
  let foo input transform_indices =
    List.iteri input ~f:(fun m arr ->
        let (_ : int) =
          List.foldi arr ~init:(-1) ~f:(fun n acc item ->
              if item > acc then (
                let m, n = transform_indices m n in
                seen.(m).(n) <- true;
                item)
              else acc)
        in
        ())
  in
  (* figuring out this mapping is a pain*)
  foo input (fun x y -> (x, y));
  foo (List.map input ~f:List.rev) (fun x y -> (x, n - y - 1));
  foo (List.transpose_exn input) (fun y x -> (x, y));
  foo
    (List.transpose_exn input |> List.map ~f:List.rev)
    (fun y x -> (m - x - 1, y));
  Array.fold seen ~init:0 ~f:(fun acc seen' ->
      Array.fold seen' ~init:0 ~f:(fun acc x -> if x then acc + 1 else acc)
      + acc)
  |> printf "%d"

let%expect_test _ =
  part1 lines;
  [%expect {| 1818 |}]

let part2 lines =
  let input =
    input lines |> List.tl_exn |> List.drop_last_exn |> List.map ~f:List.tl_exn
    |> List.map ~f:List.drop_last_exn
    |> List.to_array |> Array.map ~f:List.to_array
  in
  let dimx = Array.length input in
  let dimy = Array.length input.(0) in
  let scenic = Array.make_matrix ~dimx ~dimy 1 in
  let foo input scenic =
    Array.iteri input ~f:(fun m arr ->
        let visible = Array.create ~len:10 1 in
        let update_visible n =
          Array.iteri visible ~f:(fun i x ->
              visible.(i) <- (if i <= n then 1 else x + 1))
        in
        Array.iteri arr ~f:(fun n x ->
            scenic.(m).(n) <- scenic.(m).(n) * visible.(x);
            update_visible x))
  in
  let _, scenic =
    (* rotating both is much easier *)
    let rotate input scenic =
      Tuple2.map (input, scenic)
        ~f:
          (Array.map ~f:(fun arr ->
               Array.to_list arr |> List.rev |> List.to_array))
      |> Tuple2.map ~f:Array.transpose_exn
    in
    List.init 4 ~f:Fn.id
    |> List.fold ~init:(input, scenic) ~f:(fun (input, scenic) x ->
           let input, scenic =
             if x = 0 then (input, scenic) else rotate input scenic
           in
           foo input scenic;
           (input, scenic))
  in
  scenic
  |> Array.map ~f:(Array.reduce_exn ~f:max)
  |> Array.reduce_exn ~f:max |> printf "%d"

let%expect_test _ =
  part2 lines;
  [%expect {| 368368 |}]
