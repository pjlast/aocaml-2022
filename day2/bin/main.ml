let read_whole_file file =
  In_channel.open_text file |> In_channel.input_all |> String.split_on_char '\n'

(* Part 1 *)

type choice = Rock | Paper | Scissors

let int_of_choice c = match c with Rock -> 1 | Paper -> 2 | Scissors -> 3

let match_result me opp =
  match me with
  | Rock -> ( match opp with Rock -> 3 | Paper -> 0 | Scissors -> 6)
  | Paper -> ( match opp with Rock -> 6 | Paper -> 3 | Scissors -> 0)
  | Scissors -> ( match opp with Rock -> 0 | Paper -> 6 | Scissors -> 3)

let choice_of_string s =
  match s with
  | "A" | "X" -> Rock
  | "B" | "Y" -> Paper
  | "C" | "Z" -> Scissors
  | _ -> raise (Invalid_argument (Format.sprintf "invalid choice %s" s))

let rec sum_all l =
  match l with
  | [] -> 0
  | h :: t -> (
      sum_all t
      +
      match String.split_on_char ' ' h with
      | first :: second :: _ ->
          let opp = choice_of_string first in
          let me = choice_of_string second in
          int_of_choice me + match_result me opp
      | _ -> 0)

let () =
  let input_lines = read_whole_file "input.txt" in
  print_endline (string_of_int (sum_all input_lines))

(* Part 2 *)
(* Translate result to assumptions made in part 1 *)

type match_outcome = Win | Loss | Draw

let match_outcome_of_string s =
  match s with
  | "X" -> Loss
  | "Y" -> Draw
  | "Z" -> Win
  | _ -> raise (Invalid_argument (Format.sprintf "invalid outcome %s" s))

let get_my_choice opp out =
  match opp with
  | Rock -> ( match out with Win -> Paper | Loss -> Scissors | Draw -> Rock)
  | Paper -> ( match out with Win -> Scissors | Loss -> Rock | Draw -> Paper)
  | Scissors -> (
      match out with Win -> Rock | Loss -> Paper | Draw -> Scissors)

let rec sum_all_2 l =
  match l with
  | [] -> 0
  | h :: t -> (
      sum_all_2 t
      +
      match String.split_on_char ' ' h with
      | first :: second :: _ ->
          let opp = choice_of_string first in
          let out = match_outcome_of_string second in
          let me = get_my_choice opp out in
          int_of_choice me + match_result me opp
      | _ -> 0)

let () =
  let input_lines = read_whole_file "input.txt" in
  print_endline (string_of_int (sum_all_2 input_lines))
