type board

(** [solve board] is the list of all the possible ways to complete [board]. *)
val solve : board -> board list

(** [correct board] tests if [board] has different entries in each row, column and box. *)
val correct : board -> bool

(** [equal board1 board2] tests whether [board1] and [board2] are identical. *)
val equal : board -> board -> bool

(** [read filepath] is a board constructed from [filepath]. *)
val read : string -> board

(** [of_list_exn list] is a board constructed from [list].
    @raise [Failure] if the lst contains invalid characters or has invalid dimensions. *)
val of_list_exn : char list list -> board

(** [print board] prints [board] in a human readable way. *)
val print : board -> unit

(** [pp_print ppf board] pretty-prints [board] using the formatter [ppf]. *)
val pp_print : Format.formatter -> board -> unit
