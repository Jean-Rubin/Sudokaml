let sudoku_testable = Alcotest.testable Sudoku.pp_print Sudoku.equal

let test_correct1 () =
  let solution1 = Sudoku.read "data/test1_sol.txt" in
  Alcotest.(check bool) "Trivial solution is correct"
    true (Sudoku.correct solution1)

let test_correct3 () =
  let solution2 = Sudoku.read "data/test3_sol.txt" in
  Alcotest.(check bool) "Simple solution is correct"
    (Sudoku.correct solution2) (true)

let test_board1 () =
  let test1 = Sudoku.read "data/test1.txt" in
  let solution1 = Sudoku.read "data/test1_sol.txt" in
  Alcotest.(check sudoku_testable) "Trivial sudoku is solved"
    solution1 (List.hd (Sudoku.sudoku test1)) 

(* let test_board2 () = *)
(*   let test2 = Sudoku.read "data/test2.txt" in *)
(*   let solution2 = Sudoku.read "data/test2_sol.txt" in *)
(*   Alcotest.(check sudoku_testable) "Simple sudoku is solved" *)
(*     (List.hd (Sudoku.sudoku test2)) (solution2) *)

let () =
  Alcotest.run "Sudoku test functions"
    [
      ("Testing functions",
      [
        Alcotest.test_case "Correct Trivial" `Quick
          test_correct1;
        Alcotest.test_case "Correct Simple" `Quick
          test_correct3;
      ]);
      ("Solving small sudoku",
      [
        Alcotest.test_case "Solve Trivial" `Quick
          test_board1;
      ])
    ]
