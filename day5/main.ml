let read_whole_file file =
  In_channel.open_text file |> In_channel.input_all |> String.split_on_char '\n'

(* from https://batsov.com/articles/2022/10/24/ocaml-tips-converting-a-string-to-a-list-of-characters/ *)
let explode s = List.init (String.length s) (String.get s)

(* Part 1 *)
let skip_one line = match line with [] -> [] | _ :: t -> t

let rec parse_line line =
  let rec aux accum = function
    | [] -> accum
    | ' ' :: ' ' :: ' ' :: t -> aux (accum @ [ [] ]) (skip_one t)
    | '[' :: c :: ']' :: t -> aux (accum @ [ [ c ] ]) (skip_one t)
    | _ -> accum
  in
  aux [] line

let join_lists l1 l2 =
  let rec aux accum = function
    | [], [] -> accum
    | l1, [] -> accum @ l1
    | [], l2 -> accum @ l2
    | h1 :: t1, h2 :: t2 -> aux (accum @ [ h1 @ h2 ]) (t1, t2)
  in
  aux [] (l1, l2)

let parse_all l =
  let rec aux accum = function
    | [] -> accum
    | h :: t -> (
        match accum with
        | [] -> aux (parse_line (explode h)) t
        | accum -> aux (join_lists (parse_line (explode h)) accum) t)
  in
  aux [] l

let crates = parse_all (read_whole_file "crates_input.txt")
