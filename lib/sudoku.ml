type +'a matrix = 'a list list
(* The matrix is given by a list of its rows *)
type choices = char list

type board = char matrix

let box_size = 3
let cell_vals = ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

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
  pp_force_newline ppf ();
  pp_print_list ~pp_sep:pp_force_newline pp_print_line ppf board

let pp_print_choices ppf (c : choices) =
  let open Format in
  let sep ppf () = fprintf ppf "; " in
  fprintf ppf "@[<h>[%a]@]"
    (pp_print_list ~pp_sep:sep pp_print_char) c

let pp_print_choices_line ppf (line: choices list) = 
  let open Format in
  let space ppf () = fprintf ppf " " in
  fprintf ppf "@[<h>[ %a ]@]"
    (pp_print_list ~pp_sep:space pp_print_choices) line

let pp_print_cm ppf (cm : choices matrix) = 
  let open Format in
  fprintf ppf "@[<v>%a@]"
    (pp_print_list ~pp_sep:pp_print_newline pp_print_choices_line) cm

let read filepath =
  let to_char = List.map (fun s -> String.get s 0) in
  let in_channel = open_in filepath in
  let rec read_board in_channel ~board_acc =
  try 
    let line = input_line in_channel in
    let line_board = to_char (String.split_on_char ' ' line) in
    read_board in_channel ~board_acc:(line_board :: board_acc)
  with
  | End_of_file ->
    close_in in_channel;
    List.rev board_acc
  | e ->
    close_in in_channel;
    raise e;
  in 
  read_board in_channel ~board_acc:[]

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
  board |>
    List.map group |>
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

(* let rec cp = 
  let outer_concat lst1 lst2 =
    List.fold_left (fun acc1 elt1 ->
      List.fold_left (fun acc2 elt2 -> (elt1::elt2)::acc2) acc1 lst2) [] lst1
  in
  function
    | [] -> [[]]
    | xs::xss -> outer_concat xs (cp xss) *)

(* let matrix_cartesian_product choices = cp (List.map cp choices) *)

let single = function
  | [] -> false
  | [_] -> true
  | _ -> false

let fixed css = List.concat (List.filter single css)

let reduce (css : choices list) : choices list = 
  let rec delete fs acc = function
    | [] -> acc
    | c::cs when List.mem c fs -> delete fs acc cs
    | c::cs -> delete fs (c::acc) cs
  in
  let remove fs cs = 
    if single cs then cs else delete fs [] cs
  in
  List.map (remove (fixed css)) css

let prune_by f (cm : choices matrix) : choices matrix =
  cm |> f |> List.map reduce |> f

let prune (cm : choices matrix) : choices matrix =
  cm |>
    prune_by rows |>
    prune_by cols |>
    prune_by boxs

let safe (cm : choices matrix) =
  let nodups_fixed css = nodups (fixed css) in
  List.for_all nodups_fixed (rows cm) &&
  List.for_all nodups_fixed (cols cm) &&
  List.for_all nodups_fixed (boxs cm)

let blocked (cm : choices matrix) =
  let void cm =
    List.exists (List.exists (( = ) [])) cm
  in
  void cm ||
  not (safe cm)

let expand (cm : choices matrix) : choices matrix list =
  let rec break predicate fst = function
    | [] -> List.rev fst, []
    | cs::_ as snd when predicate cs -> List.rev fst, snd
    | cs::css -> break predicate (cs::fst) css
  in
  let min_choice (cm : choices matrix) : int = 
    cm |>
      List.map (List.map List.length) |>
      List.concat |>
      List.filter (( < ) 1) |>
      List.fold_left min (box_size * box_size + 1)
  in
  Format.printf "\nmin_choice %d \n" (min_choice cm);
  let best cs = (List.length cs = min_choice cm) in
  let (rows1, rows2) = break (List.exists best) [] cm in
  Format.printf "\nrows1 : \n%a" pp_print_cm rows1;
  Format.printf "\nrows2 : \n%a" pp_print_cm rows2;
  Format.printf "\nhd rows2 : \n%a" pp_print_choices_line (List.hd rows2);
  let (row1, row2) = break best [] (List.hd rows2) in
  Format.printf "\nrow1 : \n%a" pp_print_choices_line row1;
  Format.printf "\nrow2 : \n%a" pp_print_choices_line row2;
  let decompose c = rows1 @ [row1 @ ([c]::(List.tl row2))] @ (List.tl rows2) in
  Format.printf "\ninner row : \n%a" pp_print_choices (List.hd row2);
  List.map decompose (List.hd row2)

let rec search (cm : choices matrix) : choices matrix list =
  match cm with
  | cm when blocked cm ->
      Format.printf "Search : Case blocked\n";
      []
  | cm when List.for_all (List.for_all single) cm ->
      Format.printf "Search : Case single\n";
    [cm]
  | cm -> 
    Format.printf "\nSearch : Choices Matrix\n%a" pp_print_cm cm;
    let intermed =
    cm |>
      expand
    in
    Format.printf "\nSearch Intermed";
    Format.printf "\nNumber of choices: %d\n" (List.length intermed);
    Format.printf "\nMatrix_choice 1\n%a" pp_print_cm (List.hd intermed);
    Format.printf "\nMatrix_choice 2\n%a" pp_print_cm (List.nth intermed 1);
    Format.printf "\nMatrix_choice 1 pruned \n%a"
      pp_print_cm (prune (List.nth intermed 0));
    intermed |>
      List.map (fun cm -> search (prune cm)) |>
      List.concat

let sudoku board =
  let intermed = 
  board |>
    choices |>
    prune in
  Format.printf "%a \n" pp_print_cm intermed;
  Format.printf "%s \n" "Now back to intermed";
  intermed |>
    search |>
    (List.hd |> List.map |> List.map |> List.map)



(* let test = read "test/data/test1.txt";; *)
(* let pruned = test |> choices |> prune;; *)
(* let f x = (List.hd |> List.map |> List.map) x;; *)
(* List.map (List.map (List.map List.hd)) (search pruned);; *)
(* (* let best cs = (List.length cs = min_choice pruned);; *) *)
(* let (rows1, rows2) = break (List.exists best) [] pruned;; *)
(* pp_print_cm Format.std_formatter rows1;; *)
(* pp_print_cm Format.std_formatter rows2;; *)
(* sudoku test;; *)
