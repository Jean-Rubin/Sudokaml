let sudoku_testable = Alcotest.testable Sudoku.pp_print Sudoku.equal

let test_of_list1 () =
  let board1_read = Sudoku.read "data/test1.txt" in
  let board1_of_list = 
    Sudoku.of_list_exn
    [['2';'7';'6';'3';'1';'4';'9';'5';'.'];
     ['8';'.';'4';'9';'6';'2';'7';'1';'3'];
     ['9';'1';'3';'8';'7';'5';'2';'6';'4'];
     ['4';'6';'8';'1';'2';'7';'3';'9';'5'];
     ['5';'9';'7';'4';'3';'8';'6';'2';'1'];
     ['1';'3';'2';'5';'9';'6';'4';'8';'7'];
     ['3';'2';'5';'7';'8';'9';'1';'4';'6'];
     ['6';'4';'1';'2';'5';'3';'8';'7';'9'];
     ['7';'8';'9';'.';'4';'1';'5';'.';'2']]
  in
  Alcotest.(check sudoku_testable) "Parsing file or list produce the same board"
  board1_read board1_of_list

let test_of_list2 () =
  let invalid_board = 
    [['2';'7';'6';'3';'1';'4';'9';'5';'.'];
     ['8';'.';'4';'9';'6';'2';'7';'1';'3'];
     ['9';'1';'3';'8';'7';'5';'2';'6';'4'];
     ['4';'6';'8';'1';'2';'7';'3';'9';'5'];
     ['5';'9';'7';'4';'3';'8';'6';'2';'1'];
     ['1';'3';'2';'5';'9';'6';'4';'8';'7'];
     ['3';'2';'5';'7';'8';'9';'1';'4';'6'];
     ['7';'8';'9';'.';'4';'1';'5';'.';'2']]
  in 
  Alcotest.check_raises "Parsing list with too few rows produce an exception"
  (Failure "Could not parse list as a board") (fun () -> ignore (Sudoku.of_list_exn invalid_board))

let test_of_list3 () =
  let invalid_board = 
    [['2';'7';'6';'3';'1';'4';'9';'5';'.'];
     ['8';'.';'4';'9';'6';'2';'7';'1';'3'];
     ['9';'1';'3';'8';'7';'5';'2';'6';'4'];
     ['4';'6';'8';'1';'2';'7';'3';'9';'5'];
     ['5';'9';'7';'4';'3';'8';'6';'2';'1'];
     ['1';'3';'2';'5';'9';'6';'4';'8';'7'];
     ['3';'2';'5';'7';'8';'9';'1';'4'];
     ['6';'4';'1';'2';'5';'3';'8';'7';'9'];
     ['7';'8';'9';'.';'4';'1';'5';'.';'2']]
  in 
  Alcotest.check_raises "Parsing list with a row too short produce an exception"
  (Failure "Could not parse list as a board") (fun () -> ignore (Sudoku.of_list_exn invalid_board))

let test_of_list4 () =
  let invalid_board = 
    [['2';'7';'6';'3';'1';'4';'9';'5';'.'];
     ['8';'.';'4';'9';'6';'2';'7';'1';'3'];
     ['9';'1';'a';'8';'7';'5';'2';'6';'4'];
     ['4';'6';'8';'1';'2';'7';'3';'9';'5'];
     ['5';'9';'7';'4';'3';'8';'6';'2';'1'];
     ['1';'3';'2';'5';'9';'6';'4';'8';'7'];
     ['3';'2';'5';'7';'8';'9';'1';'4';'6'];
     ['6';'4';'1';'2';'5';'3';'8';'7';'9'];
     ['7';'8';'9';'.';'4';'1';'5';'.';'2']]
  in 
  Alcotest.check_raises "Parsing list with invalid characters produce an exception"
  (Failure "Could not parse list as a board") (fun () -> ignore (Sudoku.of_list_exn invalid_board))

let test_of_list5 () =
  let impossible_board = 
    [['2';'7';'6';'3';'1';'.';'9';'.';'.'];
     ['8';'.';'4';'9';'6';'.';'7';'1';'.'];
     ['9';'1';'4';'8';'7';'.';'2';'6';'4'];
     ['4';'.';'8';'1';'2';'.';'3';'.';'5'];
     ['5';'9';'7';'4';'3';'.';'6';'2';'1'];
     ['.';'3';'2';'1';'9';'.';'4';'8';'.'];
     ['3';'.';'5';'7';'8';'.';'1';'4';'6'];
     ['6';'4';'1';'2';'5';'.';'8';'7';'9'];
     ['7';'8';'9';'.';'4';'.';'5';'.';'.']]
  in
  ignore (Sudoku.of_list_exn impossible_board)

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
        Alcotest.test_case "Read from file or list" `Quick
          test_of_list1;
        Alcotest.test_case "Exception Too few rows" `Quick
          test_of_list2;
        Alcotest.test_case "Exception Row is too short" `Quick
          test_of_list3;
        Alcotest.test_case "Exception Invalid character" `Quick
          test_of_list4;
        Alcotest.test_case "Accept Impossible board" `Quick
          test_of_list5;
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
