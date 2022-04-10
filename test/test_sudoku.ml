let sudoku_testable = Alcotest.testable Sudoku.pp_print Sudoku.equal

let test_correct1 () =
  let solution1 = Sudoku.read "data/test1_sol.txt" in
  Alcotest.(check bool) "Trivial solution is correct"
    true (Sudoku.correct solution1)

let test_correct2 () =
  let solution2 = Sudoku.read "data/test3_sol.txt" in
  Alcotest.(check bool) "Very simple solution is correct"
    (Sudoku.correct solution2) (true)

let test_correct3 () =
  let solution2 = Sudoku.read "data/test3_sol.txt" in
  Alcotest.(check bool) "Simple solution is correct"
    (Sudoku.correct solution2) (true)

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


(* let test_board3 () = *)
(*   let test3 = Sudoku.read "data/test3.txt" in *)
(*   let solution3 = Sudoku.read "data/test3_sol.txt" in *)
(*   Alcotest.(check sudoku_testable) "Simple sudoku is solved" *)
(*     (List.hd (Sudoku.sudoku test3)) (solution3) *)

let test_board4 () = 
  let test4 = Sudoku.read "data/test4.txt" in
  let solution41 = Sudoku.read "data/test4_sol1.txt" in
  let solution42 = Sudoku.read "data/test4_sol2.txt" in
  Alcotest.(check bool) "First solution is found"
    true (List.exists (Sudoku.equal solution41) (Sudoku.sudoku test4));
  Alcotest.(check bool) "Second solution is found"
    true (List.exists (Sudoku.equal solution42) (Sudoku.sudoku test4))


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
      ]);
      ("Solving sudoku",
      [
        Alcotest.test_case "Solve Trivial" `Quick
          test_board1;
        Alcotest.test_case "Solve Very simple" `Quick
          test_board2;
        (* Alcotest.test_case "Solve Simple" `Quick *)
        (*   test_board3; *)
        Alcotest.test_case "Find multiple solutions" `Quick
          test_board4;
      ])
    ]
