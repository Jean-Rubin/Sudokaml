type +'a matrix = 'a list list
(* The matrix is given by a list of its rows *)
(* type choices = char list *)

type board = char matrix

let box_size = 2
let cell_vals = ['1'; '2'; '3'; '4']

let blank chr = (chr = '.')

let equal board1 board2 =
  List.equal (List.equal ( = )) board1 board2

let print board =
  let print_line line =
    List.iter (fun e -> print_char e; print_char ' ') line in
  List.iter (fun line -> print_line line; print_newline () ) board;;

let pp_print ppf board =
  let open Format in
  let space ppf () = fprintf ppf " " in
  let pp_print_line ppf line = 
    fprintf ppf "@[<h>%a@]"
      (pp_print_list ~pp_sep:space pp_print_char) line
  in
  pp_print_list ~pp_sep:pp_force_newline pp_print_line ppf board;;

let read filepath =
  let to_char = List.map (fun s -> String.get s 0) in
  let in_channel = open_in filepath in
  let rec reader board_acc in_channel =
  try 
    let line = input_line in_channel in
    let board_line = to_char (String.split_on_char ' ' line) in
    reader (board_line :: board_acc) in_channel
  with
  | End_of_file ->
    close_in in_channel;
    List.rev board_acc
  | e ->
    close_in in_channel;
    raise e;
  in reader [] in_channel

let rec nodups = function
  | [] -> true
  | x::xs -> not (List.mem x xs) && nodups xs

let rows board = board

let rec cols = function
  | [] -> []
  | [xs] -> List.map (fun x -> [x]) xs
  | xs::xss -> List.map2 List.cons xs (cols xss)

let group_by n lst =
  let rec aux k acc acc_total = function
  | [] -> List.rev (List.rev acc :: acc_total)
  | lst when k = n -> aux 0 [] (List.rev acc :: acc_total) lst
  | x::xs -> aux (k + 1) (x :: acc) acc_total xs
  in aux 0 [] [] lst

let group lst = group_by box_size lst
let ungroup board = List.concat board

let boxs board =
  List.map group board |>
  group |>
  List.map cols |>
  ungroup |>
  List.map ungroup

let correct board = 
  List.for_all nodups (rows board) &&
  List.for_all nodups (cols board) &&
  List.for_all nodups (boxs board)

let choose e = if blank e then cell_vals else [e]
let choices board = List.(map (map choose)) board

let rec cp = 
  let outer_concat lst1 lst2 =
    List.fold_left (fun acc1 elt1 ->
      List.fold_left (fun acc2 elt2 -> (elt1::elt2)::acc2) acc1 lst2) [] lst1
  in
  function
    | [] -> [[]]
    | xs::xss -> outer_concat xs (cp xss)

let matrix_cartesian_product choices = cp (List.map cp choices)

let sudoku board =
  choices board |>
  matrix_cartesian_product |>
  List.filter correct
