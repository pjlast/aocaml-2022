let read_whole_file file =
  In_channel.open_text file |> In_channel.input_all |> String.split_on_char '\n'

(* Part 1 *)
let rec group l accum =
  match l with
  | [] -> accum
  | h :: t -> (
      match h with
      | "" -> group t (0 :: accum)
      | _ -> (
          match accum with
          | [] -> group t [ int_of_string h ]
          | hd :: tl -> group t ((int_of_string h + hd) :: tl)))

let rec max_of_list l cur =
  match l with [] -> cur | h :: t -> max_of_list t (max h cur)

let () =
  let input_list = read_whole_file "input.txt" in
  print_endline (string_of_int (max_of_list (group input_list []) 0))

(* Part 2 *)
let rec max_3_of_list l (m1, m2, m3) =
  match l with
  | [] -> (m1, m2, m3)
  | h :: t when h > m1 -> max_3_of_list t (h, m1, m2)
  | h :: t when h > m2 -> max_3_of_list t (m1, h, m2)
  | h :: t when h > m3 -> max_3_of_list t (m1, m2, h)
  | _ :: t -> max_3_of_list t (m1, m2, m3)

let () =
  let input_list = read_whole_file "input.txt" in
  print_endline
    (string_of_int
       (let m1, m2, m3 = max_3_of_list (group input_list []) (0, 0, 0) in
        m1 + m2 + m3))
