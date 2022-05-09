type +'a matrix = 'a list list
(* The matrix is given by a list of its rows *)
type choices = char list

type board = char matrix

let box_size = 3
let cell_vals = ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9']

let blank chr = (chr = '.')

let equal board1 board2 =
  List.equal (List.equal ( = )) board1 board2

let pp_print ppf board =
  let open Format in
  let space ppf () = fprintf ppf " " in
  let vert_bar ppf () = fprintf ppf "|" in
  let hori_bar ppf () = fprintf ppf "------+-------+------" in
  let pp_print_group ~pp_sep ~pp_group_sep ~pp_v ppf l =
    let rec aux count = function
      | [] -> ()
      | line when count = 4 ->
          pp_group_sep ppf ();
          pp_sep ppf ();
          aux 1 line
      | v::vs ->
          pp_v ppf v;
          pp_sep ppf ();
          aux (count + 1) vs
    in
    aux 1 l
  in
  let pp_print_hori ppf l =
    pp_print_group ~pp_sep:space ~pp_group_sep:vert_bar ~pp_v:pp_print_char ppf l
  in
  pp_force_newline ppf ();
  pp_print_group ~pp_sep:pp_force_newline ~pp_group_sep:hori_bar ~pp_v:pp_print_hori ppf board

let print board = pp_print Format.std_formatter board

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
  in read_board in_channel ~board_acc:[]

let rec nodups = function
  | [] -> true
  | x::xs -> not (List.mem x xs) && nodups xs

let rows board = board

let rec cols = function
  | [] -> []
  | [xs] -> List.map (fun x -> [x]) xs
  | xs::xss -> List.map2 List.cons xs (cols xss)

let group lst =
  let rec aux k acc acc_total = function
  | [] -> List.rev (List.rev acc :: acc_total)
  | lst when k = box_size -> aux 0 [] (List.rev acc :: acc_total) lst
  | x::xs -> aux (k + 1) (x :: acc) acc_total xs
  in aux 0 [] [] lst

let ungroup board = List.concat board

let boxs board =
  board
  |> List.map group
  |> group
  |> List.map cols
  |> ungroup
  |> List.map ungroup

let correct board =
  List.for_all nodups (rows board) &&
  List.for_all nodups (cols board) &&
  List.for_all nodups (boxs board)

let choose e = if blank e then cell_vals else [e]
let choices board = List.(map (map choose)) board

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
  cm
  |> prune_by rows
  |> prune_by cols
  |> prune_by boxs

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
    cm 
    |> List.map (List.map List.length)
    |> List.concat
    |> List.filter (( < ) 1)
    |> List.fold_left min (box_size * box_size + 1)
  in
  let best cs = (List.length cs = min_choice cm) in
  let (rows1, rows2) = break (List.exists best) [] cm in
  let (row1, row2) = break best [] (List.hd rows2) in
  let decompose c = rows1 @ [row1 @ ([c]::(List.tl row2))] @ (List.tl rows2) in
  List.map decompose (List.hd row2)

let rec search (cm : choices matrix) : choices matrix list =
  match cm with
  | cm when blocked cm -> []
  | cm when List.for_all (List.for_all single) cm -> [cm]
  | cm ->
      cm
      |> expand
      |> List.map (fun cm -> search (prune cm))
      |> List.concat

let sudoku board =
  board
  |> choices
  |> prune
  |> search
  |> (List.hd |> List.map |> List.map |> List.map)
