type +'a matrix = 'a list list
(* The matrix is given by a list of its rows *)
type choices = char list

type board = {
  grid : char matrix;
  box_size : int;
  cell_vals : choices
}

let blank chr = (chr = '.')

let equal board1 board2 =
  List.equal (List.equal ( = )) board1.grid board2.grid

let print board =
  let print_line line =
    List.iter (fun e -> print_char e; print_char ' ') line in
  List.iter (fun line -> print_line line; print_newline () ) board.grid;;

let pp_print ppf board =
  let open Format in
  let space ppf () = fprintf ppf " " in
  let pp_print_line ppf line = 
    fprintf ppf "@[<h>%a@]"
      (pp_print_list ~pp_sep:space pp_print_char) line
  in
  pp_print_list ~pp_sep:pp_force_newline pp_print_line ppf board.grid;;

let read filepath =
  let to_char = List.map (fun s -> String.get s 0) in
  let in_channel = open_in filepath in
  let read_cell in_channel =
    try
      let line = input_line in_channel in
      let cell_vals = List.init (String.length line) (String.get line) in
      let _ = input_line in_channel in
      cell_vals
    with
      | e ->
      close_in in_channel;
      raise e;
  in
  let rec read_grid grid_acc in_channel =
  try 
    let line = input_line in_channel in
    let grid_line = to_char (String.split_on_char ' ' line) in
    read_grid (grid_line :: grid_acc) in_channel
  with
  | End_of_file ->
    close_in in_channel;
    List.rev grid_acc
  | e ->
    close_in in_channel;
    raise e;
  in 
  let cell_vals = read_cell in_channel in
  let grid = read_grid [] in_channel in
  {
    grid = grid;
    box_size = List.length cell_vals |> float_of_int |> sqrt |> int_of_float;
    cell_vals = cell_vals;
  }

let rec nodups = function
  | [] -> true
  | x::xs -> not (List.mem x xs) && nodups xs

let rows grid = grid

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

let group board lst = group_by board.box_size lst
let ungroup grid = List.concat grid

let boxs board =
  List.map (group board) board.grid |>
  group board |>
  List.map cols |>
  ungroup |>
  List.map ungroup

let correct board = 
  List.for_all nodups (rows board.grid) &&
  List.for_all nodups (cols board.grid) &&
  List.for_all nodups (boxs board)

let choose board e = if blank e then board.cell_vals else [e]
let choices board = List.(map (map (choose board))) board.grid

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
  List.map (fun grid -> {
    grid = grid;
    box_size = board.box_size;
    cell_vals = board.cell_vals;
  }) |>
  List.filter correct
