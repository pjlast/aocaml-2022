let read_whole_file file =
  In_channel.open_text file |> In_channel.input_all |> String.split_on_char '\n'

(* from https://batsov.com/articles/2022/10/24/ocaml-tips-converting-a-string-to-a-list-of-characters/ *)
let explode s = List.init (String.length s) (String.get s)

(* Part 1 *)
let skip_one line = match line with [] -> [] | _ :: t -> t

let rec parse_line line accum =
  match line with
  | [] -> accum
  | ' ' :: ' ' :: ' ' :: t -> parse_line (skip_one t) (accum @ [ [] ])
  | '[' :: c :: ']' :: t -> parse_line (skip_one t) (accum @ [ [ c ] ])
  | _ -> accum

let rec join_lists l1 l2 accum =
  match (l1, l2) with
  | [], [] -> accum
  | l1, [] -> accum @ l1
  | [], l2 -> accum @ l2
  | h1 :: t1, h2 :: t2 -> join_lists t1 t2 (accum @ [ h1 @ h2 ])

let rec parse_all l accum =
  match l with
  | [] -> accum
  | h :: t -> (
      match accum with
      | [] -> parse_all t (parse_line (explode h) [])
      | accum -> parse_all t (join_lists accum (parse_line (explode h) []) []))

let crates = parse_all (read_whole_file "crates_input.txt") []
