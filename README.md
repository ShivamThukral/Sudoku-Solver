# Sudoku-Solver
Sudokus are fun to solve - but can we find a solution using SAT solver? 

SAT-solvers are designed to solve the Boolean Satisfiability Problem that seeks to find a truth assignment for each of a set of boolean variables for which the whole expression is true.

SAT-solvers require the boolean expression to be in Conjunctive Normal Form (CNF). An expression is in CNF if it is a conjunction of clauses, where a clause is a disjunction of literals; otherwise put, it is an AND of ORs. For example the expression -(X1+X2) is not in CNF but, 
after apply de Morgan’s Law, is transformed into -X1.-X2 it is in CNF.

Sudoku requires assignments as numerical values in each of the 81 cells where as SAT-solvers use boolean variables that are either true or false. 
The Sudoku needs to be transformed into a boolean expression in CNF to be solved. 

We can do this in two parts. Consider first a simple case where we have two variables X1 and X2 and want to enforce that exactly one is true. We can say that at least one variable must be true (X1+X2) and also that both cannot be true -(X1.X2) which is a logical XOR.
We can generalize this to N variables, X1, X2, ... ,XN by saying that as least on must be true X1+X2+ ... +XN and that no pair may both be true, -(X1.X2) . -(X2.X3) . ... . which conveniently happens to be in CNF.

#### Exactly One
**Function:** 
```exactly_one(args...)``` that takes one more arguments. Each argument should be of type  Bool, SymBool, Literal, or Variable. Lets call them ```a1, a2, . . . . ``` 
![title](images/exact_one_testcases.png)
**Output:**  ```exactly_one(args...)``` returns true if exactly one of ```a1, a2, . . .``` is true.

![title](images/exactly_one.png)

Also, these literals can be converted into CNF representation using the line: 
```julia
print(cnf(clausify(exactly_one(x, y, z)).clauses))
```
#### Sudoku
Armed with this we can generate the standard Sudoku constraints that the numbers 1 to 9 must appear in each row, each column and each 3x3 sub-matrix exactly ones. The final CNF is massive.

A Sudoku puzzle is represented as a N × N array of integers. To describe a puzzle, 0 indicates a blank square, and the integers 1 though 9 represent squares filled with that value.

**Function:**
```sudoku_solve(puzzle::Array{Integer,2})::Union{Array{Integer,2},UnSAT}``` returns a N × N array of integers if the puzzle is solvable – this should be a solution to the puzzle. If the puzzle is unsolvable, return UnSAT.
**Input**
![pic1](images/sudoko_test.png)
**Outupt**
![title](images/sudoko_solve.png)

**Input: 9x9 test cases**
![title](images/9x9.png)
**Outupt**
![title](images/9x9_sol.png)
