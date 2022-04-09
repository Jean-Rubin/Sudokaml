type board

(** [sudoku board] will generate the list of all the possible ways to complete [board] *)
val sudoku : board -> board list

(** [correct board] will test whether a filled [board] has different entries in each row, column and box *)
val correct : board -> bool

(** [equal board1 board2] will test whether [board1] and [board2] are identical *)
val equal : board -> board -> bool

(** [read filepath] will construct a board from [filepath] *)
val read : string -> board

(** [print board] will print [board] in a human readable way *)
val print : board -> unit

(** [pp_print ppf board] will pretty-print [board] using the formatter ppf *)
val pp_print : Format.formatter -> board -> unit
