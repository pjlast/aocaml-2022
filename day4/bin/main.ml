let read_whole_file file =
  In_channel.open_text file |> In_channel.input_all |> String.split_on_char '\n'

(* Part 1 *)
let range_of_string s =
  match String.split_on_char '-' s with
  | first :: second :: _ -> (int_of_string first, int_of_string second)
  | _ ->
      raise
        (Invalid_argument
           (Format.sprintf "range could not be extracted from %s" s))

let split_range s =
  match String.split_on_char ',' s with
  | first :: second :: _ -> (first, second)
  | _ ->
      raise (Invalid_argument (Format.sprintf "could not split on , in %s" s))

let ranges_contained r1 r2 =
  let r11, r12 = r1 in
  let r21, r22 = r2 in
  if r11 >= r21 && r11 <= r22 && r12 >= r21 && r12 <= r22 then 1
  else if r21 >= r11 && r21 <= r12 && r22 >= r11 && r22 <= r12 then 1
  else 0

let rec part1 l accum =
  match l with
  | [] -> accum
  | "" :: _ -> accum
  | h :: t ->
      let elf1, elf2 = split_range h in
      let r1 = range_of_string elf1 in
      let r2 = range_of_string elf2 in
      part1 t accum + ranges_contained r1 r2

let () = print_endline (string_of_int (part1 (read_whole_file "input.txt") 0))

(* Part 2 *)
let ranges_overlap r1 r2 =
  let r11, r12 = r1 in
  let r21, r22 = r2 in
  if r11 >= r21 && r11 <= r22 then 1
  else if r12 >= r21 && r12 <= r22 then 1
  else if r21 >= r11 && r21 <= r12 then 1
  else if r22 >= r11 && r22 <= r12 then 1
  else 0

let rec part2 l accum =
  match l with
  | [] -> accum
  | "" :: _ -> accum
  | h :: t ->
      let elf1, elf2 = split_range h in
      let r1 = range_of_string elf1 in
      let r2 = range_of_string elf2 in
      part2 t accum + ranges_overlap r1 r2

let () = print_endline (string_of_int (part2 (read_whole_file "input.txt") 0))
