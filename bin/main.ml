open Sudoku

let () =
  let board = read "data/board.txt" in
  List.iter print (solve board)
