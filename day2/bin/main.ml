let read_whole_file file =
  In_channel.open_text file |> In_channel.input_all |> String.split_on_char '\n'

(* Part 1 *)
let calc_round opp me =
  match opp with
  | "A" -> ( match me with "X" -> 4 | "Y" -> 8 | "Z" -> 3 | _ -> 0)
  | "B" -> ( match me with "X" -> 1 | "Y" -> 5 | "Z" -> 9 | _ -> 0)
  | "C" -> ( match me with "X" -> 7 | "Y" -> 2 | "Z" -> 6 | _ -> 0)
  | _ -> 0

let rec sum_all l =
    match l with
    | [] -> 0
    | h :: t -> (
        sum_all t
        +
        match String.split_on_char ' ' h with
        | first :: second :: _ -> calc_round first second
        | _ -> 0)

let () =
    let input_lines = read_whole_file "input.txt" in
    print_endline (string_of_int (sum_all input_lines))

(* Part 2 *)
(* Translate result to assumptions made in part 1 *)
let subst_me opp res =
  match opp with
  | "A" -> ( match res with "X" -> "Z" | "Y" -> "X" | "Z" -> "Y" | _ -> "")
  | "B" -> ( match res with "X" -> "X" | "Y" -> "Y" | "Z" -> "Z" | _ -> "")
  | "C" -> ( match res with "X" -> "Y" | "Y" -> "Z" | "Z" -> "X" | _ -> "")
  | _ -> ""

let rec sum_all_2 l =
    match l with
    | [] -> 0
    | h :: t -> (
        sum_all_2 t
        +
        match String.split_on_char ' ' h with
        | first :: second :: _ -> calc_round first (subst_me first second)
        | _ -> 0)

let () =
    let input_lines = read_whole_file "input.txt" in
    print_endline (string_of_int (sum_all_2 input_lines))
