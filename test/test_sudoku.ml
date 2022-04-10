let sudoku_testable = Alcotest.testable Sudoku.pp_print Sudoku.equal

let test_correct1 () =
  let solution1 = Sudoku.read "data/test1_sol.txt" in
  Alcotest.(check bool) "4x4 Solution is correct"
    (Sudoku.correct solution1) (true)

(* let test_correct2 () = *)
(*   let solution2 = Sudoku.read "data/test2_sol.txt" in *)
(*   Alcotest.(check bool) "9x9 Solution is correct" *)
(*     (Sudoku.correct solution2) (true) *)

let test_board1 () =
  let test1 = Sudoku.read "data/test1.txt" in
  let solution1 = Sudoku.read "data/test1_sol.txt" in
  Alcotest.(check sudoku_testable) "Simple 4x4 Sudoku is solved"
    (List.hd (Sudoku.sudoku test1)) (solution1)

(* let test_board2 () = *)
(*   let test2 = Sudoku.read "data/test2.txt" in *)
(*   let solution2 = Sudoku.read "data/test2_sol.txt" in *)
(*   Alcotest.(check sudoku_testable) "Simple 9x9 Sudoku is solved" *)
(*     (List.hd (Sudoku.sudoku test2)) (solution2) *)

let () =
  Alcotest.run "Sudoku test functions"
    [
      ("Testing functions",
      [
        Alcotest.test_case "Correct 4x4" `Quick
          test_correct1;
        (* Alcotest.test_case "Correct 9x9" `Quick *)
        (*   test_correct2; *)
      ]);
      ("Solving small sudoku",
      [
        Alcotest.test_case "Simple 4x4" `Quick
          test_board1;
      ])
    ]
