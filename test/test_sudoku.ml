let sudoku_testable = Alcotest.testable Sudoku.pp_print Sudoku.equal

let test_of_list_correct () =
  let board_read = Sudoku.read "data/trivial.txt" in
  let board_of_list = 
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
  board_read board_of_list

let test_of_list_raises ~invalid_lst ~failure ~msg =
  let parse_invalid () = ignore (Sudoku.of_list_exn invalid_lst) in
  Alcotest.check_raises msg (Failure failure) parse_invalid

let test_of_list_too_few_rows () =
  test_of_list_raises
    ~invalid_lst:
    [['2';'7';'6';'3';'1';'4';'9';'5';'.'];
     ['8';'.';'4';'9';'6';'2';'7';'1';'3'];
     ['9';'1';'3';'8';'7';'5';'2';'6';'4'];
     ['4';'6';'8';'1';'2';'7';'3';'9';'5'];
     ['5';'9';'7';'4';'3';'8';'6';'2';'1'];
     ['1';'3';'2';'5';'9';'6';'4';'8';'7'];
     ['3';'2';'5';'7';'8';'9';'1';'4';'6'];
     ['7';'8';'9';'.';'4';'1';'5';'.';'2']]
    ~failure:"Could not parse list as a board"
    ~msg:"Parsing list with too few rows produce an exception"

let test_of_list_row_too_short () =
  test_of_list_raises
    ~invalid_lst:
    [['2';'7';'6';'3';'1';'4';'9';'5';'.'];
     ['8';'.';'4';'9';'6';'2';'7';'1';'3'];
     ['9';'1';'3';'8';'7';'5';'2';'6';'4'];
     ['4';'6';'8';'1';'2';'7';'3';'9';'5'];
     ['5';'9';'7';'4';'3';'8';'6';'2';'1'];
     ['1';'3';'2';'5';'9';'6';'4';'8';'7'];
     ['3';'2';'5';'7';'8';'9';'1';'4'];
     ['6';'4';'1';'2';'5';'3';'8';'7';'9'];
     ['7';'8';'9';'.';'4';'1';'5';'.';'2']]
    ~failure:"Could not parse list as a board"
    ~msg:"Parsing list with a row too short produce an exception"

let test_of_list_invalid_characters () =
  test_of_list_raises
    ~invalid_lst:
    [['2';'7';'6';'3';'1';'4';'9';'5';'.'];
     ['8';'.';'4';'9';'6';'2';'7';'1';'3'];
     ['9';'1';'a';'8';'7';'5';'2';'6';'4'];
     ['4';'6';'8';'1';'2';'7';'3';'9';'5'];
     ['5';'9';'7';'4';'3';'8';'6';'2';'1'];
     ['1';'3';'2';'5';'9';'6';'4';'8';'7'];
     ['3';'2';'5';'7';'8';'9';'1';'4';'6'];
     ['6';'4';'1';'2';'5';'3';'8';'7';'9'];
     ['7';'8';'9';'.';'4';'1';'5';'.';'2']]
    ~failure:"Could not parse list as a board"
    ~msg:"Parsing list with invalid characters produce an exception"

let test_of_list_impossible () =
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

let test_correct ~filepath ~value ~msg =
  let solution = Sudoku.read filepath in
  Alcotest.(check bool) msg value (Sudoku.correct solution)

let test_correct_trivial () =
  test_correct
    ~filepath:"data/trivial_sol.txt"
    ~value:true
    ~msg:"Trivial solution is correct"

let test_correct_very_simple () =
  test_correct
    ~filepath:"data/very_simple_sol.txt"
    ~value:true
    ~msg:"Very simple solution is correct"

let test_correct_simple () =
  test_correct
    ~filepath:"data/simple_sol.txt"
    ~value:true
    ~msg:"Simple solution is correct"

let test_correct_wrong_simple () =
  test_correct
    ~filepath:"data/wrong_simple.txt"
    ~value:false
    ~msg:"Proposed grid is incorrect"

let test_board_unique ~input_path ~sol_path ~msg =
  let input = Sudoku.read input_path in
  let solution = Sudoku.read sol_path in
  Alcotest.(check sudoku_testable) msg solution (List.hd (Sudoku.solve input))

let test_board_trivial () =
  test_board_unique
    ~input_path:"data/trivial.txt"
    ~sol_path:"data/trivial_sol.txt"
    ~msg:"Trivial sudoku is solved"

let test_board_very_simple () =
  test_board_unique
    ~input_path:"data/very_simple.txt"
    ~sol_path:"data/very_simple_sol.txt"
    ~msg:"Very simple sudoku is solved"

let test_board_simple () =
  test_board_unique
    ~input_path:"data/simple.txt"
    ~sol_path:"data/simple_sol.txt"
    ~msg:"Simple sudoku is solved"

let test_board_very_difficult () =
  test_board_unique
    ~input_path:"data/very_difficult.txt"
    ~sol_path:"data/very_difficult_sol.txt"
    ~msg:"Very difficult sudoku is solved"

let test_board_multiple () = 
  let input_multiple = Sudoku.read "data/multiple.txt" in
  let solution_1_multiple = Sudoku.read "data/multiple_sol1.txt" in
  let solution_2_multiple = Sudoku.read "data/multiple_sol2.txt" in
  Alcotest.(check bool) "First solution is found"
    true (List.exists (Sudoku.equal solution_1_multiple) (Sudoku.solve input_multiple));
  Alcotest.(check bool) "Second solution is found"
    true (List.exists (Sudoku.equal solution_2_multiple) (Sudoku.solve input_multiple))

let test_board_impossible () = 
  let input_impossible = Sudoku.read "data/impossible.txt" in
  Alcotest.(check bool) "No solutions are found"
    true ((Sudoku.solve input_impossible) = [])

let () =
  Alcotest.run "Sudoku test functions"
    [
      ("Testing functions",
      [
        Alcotest.test_case "Read from file or list" `Quick
          test_of_list_correct;
        Alcotest.test_case "Exception Too few rows" `Quick
          test_of_list_too_few_rows;
        Alcotest.test_case "Exception Row is too short" `Quick
          test_of_list_row_too_short;
        Alcotest.test_case "Exception Invalid character" `Quick
          test_of_list_invalid_characters;
        Alcotest.test_case "Accept Impossible board" `Quick
          test_of_list_impossible;
        Alcotest.test_case "Correct Trivial" `Quick
          test_correct_trivial;
        Alcotest.test_case "Correct Very simple" `Quick
          test_correct_very_simple;
        Alcotest.test_case "Correct Simple" `Quick
          test_correct_simple;
        Alcotest.test_case "Incorrect Simple" `Quick
          test_correct_wrong_simple;
      ]);
      ("Solving sudoku",
      [
        Alcotest.test_case "Solve Trivial" `Quick
          test_board_trivial;
        Alcotest.test_case "Solve Very simple" `Quick
          test_board_very_simple;
        Alcotest.test_case "Solve Simple" `Slow
          test_board_simple;
        Alcotest.test_case "Solve Very difficult" `Slow
          test_board_very_difficult;
        Alcotest.test_case "Find multiple solutions" `Quick
          test_board_multiple;
        Alcotest.test_case "No solutions are found" `Slow
          test_board_impossible;
      ])
    ]
