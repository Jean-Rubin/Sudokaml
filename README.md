# Sudokaml

Sudoku solver in Ocaml based on the [functional pearl written by Richard Bird](https://www.cs.tufts.edu/~nr/cs257/archive/richard-bird/sudoku.pdf).

# Build

Execute the command in your terminal:
```bash
dune build
```

# Run

Fill in the board you wish to solve in `data/board.txt`.
It must be space separated using the values from 1 to 9, with each row containing 9 elements, like in the example.
Missing values must be filled with a dot `.`.

You can then simply run the command:
```
dune exec bin/main.exe
```
that will execute the file `bin/main.ml`.
