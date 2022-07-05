# futoshiki-solver

This project is a script that allows you to solve futoshiki puzzles that initially don't have numbers. The principle used is "attempt-fail", checking in each square if the number is possible to insert, the solution presented is always the smallest lexicographically.

![GitHub last commit](https://img.shields.io/github/last-commit/Pobedinsky/futoshiki-solver)

## Example

After executing the file

Input example:
```
4
3
0 0 1 0
0 1 0 0
2 1 3 1
```
Output:
```
2 3 1 4 
1 2 4 3 
3 4 2 1 
4 1 3 2
```

The first line of input is the square dimension of the table (nxn). 

The second line of input is the number of restrictions.
The next lines are the restrictions; the square with coordinates (0,0) should be bigger than (1,0) etc. 

The output is the board with the solution to the puzzle, if there is no solution, would appear "IMPOSSIBLE"
