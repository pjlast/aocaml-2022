let read_whole_file file =
  In_channel.open_text file |> In_channel.input_all |> String.split_on_char '\n'

(* Part 1 *)
let split line =
  ( String.sub line 0 (String.length line / 2),
    String.sub line (String.length line / 2) (String.length line / 2) )

let to_points c =
  match int_of_char c with
  | c when c >= int_of_char 'a' -> c + 1 - int_of_char 'a'
  | c -> c + 27 - int_of_char 'A'

let rec common_item line1 line2 =
  match line1 with
  | [] -> 0
  | h :: t -> (
      match String.contains line2 h with
      | true -> to_points h
      | false -> common_item t line2)

(* from https://batsov.com/articles/2022/10/24/ocaml-tips-converting-a-string-to-a-list-of-characters/ *)
let explode s = List.init (String.length s) (String.get s)

let rec sum l =
  match l with
  | [] -> 0
  | "" :: _ -> 0
  | h :: t ->
      sum t
      +
      let line1, line2 = split h in
      common_item (explode line1) line2

let () =
  let input_lines = read_whole_file "input.txt" in
  print_endline (string_of_int (sum input_lines))

(* Part 2 *)
let rec common_item_3 line1 line2 line3 =
  match line1 with
  | [] -> 0
  | h :: t -> (
      match String.contains line2 h && String.contains line3 h with
      | true -> to_points h
      | false -> common_item_3 t line2 line3)

let rec sum2 l =
  match l with
  | [] -> 0
  | _ :: [] -> 0
  | [ _; _ ] -> 0
  | first :: second :: third :: t ->
      sum2 t + common_item_3 (explode first) second third

let () =
  let input_lines = read_whole_file "input.txt" in
  print_endline (string_of_int (sum2 input_lines))
