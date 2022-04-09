let sudoku_testable = Alcotest.testable Sudoku.pp_print Sudoku.equal

let test_correct1 () =
  let solution1 = Sudoku.read "data/test1sol.txt" in
  Alcotest.(check bool) "Solution is correct"
    (Sudoku.correct solution1) (true)

let test_board1 () =
  let test1 = Sudoku.read "data/test1.txt" in
  let solution1 = Sudoku.read "data/test1sol.txt" in
  Alcotest.(check sudoku_testable) "Simple Sudoku is solved"
    (List.hd (Sudoku.sudoku test1)) (solution1)

let () =
  Alcotest.run "Sudoku test functions"
    [
      ("Testing functions",
      [
        Alcotest.test_case "Correct 4x4" `Quick
          test_correct1;
      ]);
      ("Solving small sudoku",
      [
        Alcotest.test_case "Simple 4x4" `Quick
          test_board1;
      ])
    ]
