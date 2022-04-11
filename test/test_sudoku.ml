let sudoku_testable = Alcotest.testable Sudoku.pp_print Sudoku.equal

let test_correct1 () =
  let solution1 = Sudoku.read "data/test1_sol.txt" in
  Alcotest.(check bool) "Trivial solution is correct"
    true (Sudoku.correct solution1)

let test_correct2 () =
  let solution2 = Sudoku.read "data/test3_sol.txt" in
  Alcotest.(check bool) "Very simple solution is correct"
    true (Sudoku.correct solution2)

let test_correct3 () =
  let solution3 = Sudoku.read "data/test3_sol.txt" in
  Alcotest.(check bool) "Simple solution is correct"
    true (Sudoku.correct solution3)

let test_correct7 () =
  let solution7 = Sudoku.read "data/test7.txt" in
  Alcotest.(check bool) "Proposed grid is incorrect"
    false (Sudoku.correct solution7)

let test_board1 () =
  let test1 = Sudoku.read "data/test1.txt" in
  let solution1 = Sudoku.read "data/test1_sol.txt" in
  Alcotest.(check sudoku_testable) "Trivial sudoku is solved"
    solution1 (List.hd (Sudoku.sudoku test1)) 

let test_board2 () =
  let test2 = Sudoku.read "data/test2.txt" in
  let solution2 = Sudoku.read "data/test2_sol.txt" in
  Alcotest.(check sudoku_testable) "Very simple sudoku is solved"
    solution2 (List.hd (Sudoku.sudoku test2)) 

let test_board3 () =
  let test3 = Sudoku.read "data/test3.txt" in
  let solution3 = Sudoku.read "data/test3_sol.txt" in
  Alcotest.(check sudoku_testable) "Simple sudoku is solved"
    (List.hd (Sudoku.sudoku test3)) (solution3)

let test_board4 () = 
  let test4 = Sudoku.read "data/test4.txt" in
  let solution41 = Sudoku.read "data/test4_sol1.txt" in
  let solution42 = Sudoku.read "data/test4_sol2.txt" in
  Alcotest.(check bool) "First solution is found"
    true (List.exists (Sudoku.equal solution41) (Sudoku.sudoku test4));
  Alcotest.(check bool) "Second solution is found"
    true (List.exists (Sudoku.equal solution42) (Sudoku.sudoku test4))

let test_board5 () = 
  let test5 = Sudoku.read "data/test5.txt" in
  Alcotest.(check bool) "No solutions are found"
    true ((Sudoku.sudoku test5) = [])

let test_board6 () =
  let test6 = Sudoku.read "data/test6.txt" in
  let solution6 = Sudoku.read "data/test6_sol.txt" in
  Alcotest.(check sudoku_testable) "Very difficult sudoku is solved"
    (List.hd (Sudoku.sudoku test6)) (solution6)

let () =
  Alcotest.run "Sudoku test functions"
    [
      ("Testing functions",
      [
        Alcotest.test_case "Correct Trivial" `Quick
          test_correct1;
        Alcotest.test_case "Correct Very simple" `Quick
          test_correct2;
        Alcotest.test_case "Correct Simple" `Quick
          test_correct3;
        Alcotest.test_case "Incorrect Simple" `Quick
          test_correct7;
      ]);
      ("Solving sudoku",
      [
        Alcotest.test_case "Solve Trivial" `Quick
          test_board1;
        Alcotest.test_case "Solve Very simple" `Quick
          test_board2;
        Alcotest.test_case "Solve Simple" `Slow
          test_board3;
        Alcotest.test_case "Solve Very difficult" `Slow
          test_board6;
        Alcotest.test_case "Find multiple solutions" `Quick
          test_board4;
        Alcotest.test_case "No solutions are found" `Slow
          test_board5;
      ])
    ]
