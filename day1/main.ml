let read_whole_file file =
  In_channel.open_text file |> In_channel.input_all |> String.split_on_char '\n'

(* Part 1 *)
let group l =
  let rec aux accum = function
    | [] -> accum
    | h :: t -> (
        match h with
        | "" -> aux (0 :: accum) t
        | _ -> (
            match accum with
            | [] -> aux [ int_of_string h ] t
            | hd :: tl -> aux ((int_of_string h + hd) :: tl) t))
  in
  aux [] l

let max_of_list l =
  let rec aux cur = function [] -> cur | h :: t -> aux (max h cur) t in
  aux 0 l

let () =
  let input_list = read_whole_file "input.txt" in
  print_endline (string_of_int (max_of_list (group input_list)))

(* Part 2 *)
let max_3_of_list l =
  let rec aux (m1, m2, m3) = function
    | [] -> (m1, m2, m3)
    | h :: t when h > m1 -> aux (h, m1, m2) t
    | h :: t when h > m2 -> aux (m1, h, m2) t
    | h :: t when h > m3 -> aux (m1, m2, h) t
    | _ :: t -> aux (m1, m2, m3) t
  in
  aux (0, 0, 0) l

let () =
  let input_list = read_whole_file "input.txt" in
  print_endline
    (string_of_int
       (let m1, m2, m3 = max_3_of_list (group input_list) in
        m1 + m2 + m3))
