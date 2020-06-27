include("symbool.jl")

x = Variable("x")
y = Variable("y")
z = Variable("z")
t = Variable("t")
p = Variable("p")
q = Variable("q")
r = Variable("r")
s = Variable("s")


function exactly_one_suboptimal(args...)
  symbool_array = Array{SymBool,1}([])
  for k in args
    if(k == undef_bool)
      return UndefBool() # undefined bool return struct
    end
    fixed_value = SymBoolFix(k); # this can be a bool or a literal
    push!(symbool_array,fixed_value)
  end
  cl1 = reduce(or,symbool_array) # first or the symbools
  n = length(symbool_array) # then not (x and y) for all elements pairwise
  clauses = Array{SymBool,1}([])
  for i in 1:n
    for j in (i+1):n
      clause = not(and(symbool_array[i],symbool_array[j]))
      push!(clauses,clause)
    end
  end
  cnf = reduce(and,clauses)
  cnf = and(cnf,cl1)
  return cnf
end

function exactly_one_optimal(args...)
  if(length(args)==0)
    return false
  end
  symbool_array = Array{SymBool,1}([])   #convert everything to symbool
  for k in args
    if(k == undef_bool)
      return UndefBool() # undefined bool return struct
    end
    fixed_value = SymBoolFix(k); # this can be a bool or a literal
    push!(symbool_array,fixed_value)
  end
  zero = Array{SymBool,1}([])
  one = Array{SymBool,1}([])
  push!(zero,!symbool_array[1])
  push!(one,symbool_array[1])

  for i in 2:length(symbool_array)
    push!(zero,and(zero[i-1],!symbool_array[i]))
    push!(one,or(and(zero[i-1],symbool_array[i]),and(one[i-1],!symbool_array[i])))
  end
  return one[length(one)]
end

xandy = and(x,y)
xory = or(x,y)
xandy_or_yandz= or(and(x,y),and(y,z)) # symbool
#Given test cases
println("Exactly one returns = ", exactly_one_optimal(x,y,z)(Model(x=>true,y=>false,z=>false)) , "\n") # T1
println("Exactly one returns = ", exactly_one_optimal(x,y,z)(Model(x=>true,y=>false,z=>true)) , "\n") # T2
println("Exactly one returns = ", exactly_one_optimal(x,y,z)(Model(x=>false,y=>true)) , "\n") # T3
#Added test cases
println("Exactly one returns = ", exactly_one_optimal(xandy,true,false)(Model(x=>false,y=>true)) , "\n") # T4
println("Exactly one returns = ", exactly_one_optimal(xandy,xory,false)(Model(x=>true,y=>true)), "\n") # T5
println("Exactly one returns = ", exactly_one_optimal(Literal(:not,x) , Literal(y), false)(Model(x=>true,y=>true)) , "\n") #T6
println("Exactly one returns = ", exactly_one_optimal(Literal(:not,x) , Literal(y), z)(Model(x=>true,y=>true)) , "\n") #T7



println("N = 1 Variables = ",length(vars(cnf(clausify(exactly_one_optimal(x)).clauses))))
println("N = 2 Variables = ",length(vars(cnf(clausify(exactly_one_optimal(x,y)).clauses))))
println("N = 3 Variables = ",length(vars(cnf(clausify(exactly_one_optimal(x,y,z)).clauses))))
println("N = 4 Variables = ",length(vars(cnf(clausify(exactly_one_optimal(x,y,z,t)).clauses))))
println("N = 5 Variables = ",length(vars(cnf(clausify(exactly_one_optimal(x,y,z,t,p)).clauses))))
println("N = 6 Variables = ",length(vars(cnf(clausify(exactly_one_optimal(x,y,z,t,p,q)).clauses))))
println("N = 7 Variables = ",length(vars(cnf(clausify(exactly_one_optimal(x,y,z,t,p,q,r)).clauses))))
println("N = 8 Variables = ",length(vars(cnf(clausify(exactly_one_optimal(x,y,z,t,p,q,r,s)).clauses))))




# symbool_flatten takes an arbitrarily nested array of SymBools
# and returns a one-dimensional array (i.e. a vector) with the same
# elements.  I found this to be helpful when implementing my sudoku_solve
# function, but you don't have to use these functions if you don't need
# them in your approach.
function symbool_flatten(a::Array{SymBool,1}, x)::Array{SymBool,1}
  if(typeof(x) <: Array)
    for y in x symbool_flatten(a, y) end
    a
  else push!(a, x)
  end
end

function symbool_flatten(x)::Array{SymBool,1}
  a = Array{SymBool,1}([])
  symbool_flatten(a, x)
end


# I also found it helpful to write a function that generates the constraints
# for a completely blank sudoku, then add the constraints for a particular
# sudoku puzzle later
function blank_sudoku(n=9,M=3)
  # All the variables we need: each cell has one of the 9 digits
  lits = []
  for i in 1:n
    line = []
    for j in 1:n
      column = []
      for k in 1:n
        IJK = string("x",repr(i),"",repr(j),"",repr(k))
        push!(column,Variable(string(IJK)))
      end
      push!(line,column)
    end
    push!(lits,line)
  end
  # Set of contraints #1: a cell has only one value.
  cellconstraint=[]
  for i in 1:n
    for j in 1:n
      cell = [lits[i][j][k] for k in 1:n]
      #print(Clause(lits[i][j]),"\n")
      if(cellconstraint == [])
        cellconstraint = exactly_one_optimal(cell...)
      else
        cellconstraint = and(cellconstraint,exactly_one_optimal(cell...))
      end
    end
  end
  # Set of constraints #2: each value is used only once in a row.
  row_consts = []
  for j in 1:n
    for k in 1:n
      row_const = [lits[i][j][k] for i in 1:n]
      if(row_consts == [])
        row_consts = exactly_one_optimal(row_const...)
      else
        row_consts = and(row_consts,exactly_one_optimal(row_const...))
      end
    end
  end
  # Set of constraints #3: each value used exactly once in each column:
  col_consts = []
  for i in 1:n
    for k in 1:n
      col_const = [lits[i][j][k] for j in 1:n]
      if(col_consts==[])
        col_consts = exactly_one_optimal(col_const...)
      else
        col_consts = and(col_consts,exactly_one_optimal(col_const...))
      end
    end
  end
  # Set of constraints #4: each value used exaclty once in each 3x3 grid.
  tile_consts = []
  if(M !=0 )
    for k in 1:n
      for i in 1:M:n
        for j in 1:M:n
          tile_cell_const = []
          for a in 0:M-1
            for b in 0:M-1
              #  print((i+a)," ",(j+b),"\n")
              push!(tile_cell_const,lits[i+a][j+b][k])
            end
          end
          if(tile_consts==[])
            tile_consts = exactly_one_optimal(tile_cell_const...)
          else
            tile_consts = and(tile_consts,exactly_one_optimal(tile_cell_const...))
          end
        end
      end
    end
    return and(cellconstraint,col_consts,row_consts,tile_consts)
  end
  return and(cellconstraint,col_consts,row_consts)
end

function sudoku_solve(puzzle::Array{Integer,2})::Union{Array{Integer,2},UnSAT}
  (n,d) = size(puzzle)
  matrix_constraints = []

  indices = findall(x->x<0,puzzle)
  for i in indices
    puzzle[i] *= -1
    consts = Variable(string("x",repr(i[1]),repr(i[2]),repr(puzzle[i])))
    #println(consts.name)
    if(matrix_constraints == [])
      matrix_constraints = !consts
    else
      matrix_constraints = or(matrix_constraints,!consts)
    end
    puzzle[i] = 0
  end
  for i in 1:n
    for j in 1:d
      if(puzzle[i,j] != 0)
        consts = Variable(string("x",repr(i),repr(j),repr(puzzle[i,j])))
        if(matrix_constraints == [])
          matrix_constraints = consts
        else
          matrix_constraints = and(matrix_constraints,consts)
        end
      end
    end
  end

  clauses = and(matrix_constraints,blank_sudoku(n,3))  ### PLEASE CHANGE THIS FOR EACH SUDOKU
  s = solve(eq(true,clauses))
  if (s == unsat)
    return unsat
  end
  (n,d) = size(puzzle)
  for (key,value) in s
    if(value == true)
      row = parse(Int,key.name[2])
      col = parse(Int,key.name[3])
      value = parse(Int,key.name[4])
      puzzle[row,col] = value
    end
  end
  return puzzle
end

function sudoku_solve_runner(puzzle::Array{Integer,2})::Array{Integer,3}
  flag = true
  (n,d) = size(puzzle)
  all_solutions = []
  indices = findall(x->x==0,puzzle) # find all zeros indeces
  while(flag)
    solution = sudoku_solve(puzzle)
    if solution == unsat
      if all_solutions == []
        return zeros(Int,(n,n,0))
      else
        print(solution)
        return all_solutions
      end
    end
    # append this solution to all solutions
    if all_solutions == []
      all_solutions = cat(solution,dims=3)
    else
      all_solutions = cat(all_solutions,solution,dims=3)
    end
    # change in the current solution and negate all the values which we found
    for i in indices
      solution[i] *= -1
    end
    println(solution)
    # update the puzzle
    puzzle = solution
  end

end

#=function sudoku_solve_runner(puzzle::Array{Integer,2})::Array{Integer,3}
  flag = true
  (n,d) = size(puzzle)
  previous_solution = []
  all_solutions = []
  while(flag)
    solution = sudoku_solve(puzzle)
    print(solution)
    if solution == unsat
      return zeros(Int,(n,n,0))
    end
    if(previous_solution == solution)
      print("\nyes\n")
      flag = false
    else
      if all_solutions == []
        all_solutions = cat(solution,dims=3)
      else
        all_solutions = cat(all_solutions,solution,dims=3)
      end
      previous_solution = solution
    end
  end
  return all_solutions

end=#

function sudoku1()::Array{Integer,2}
  [ [0 8 0   5 3 1   0 2 4];
  [1 0 0   0 9 0   0 0 0];
  [9 2 5   7 0 0   0 0 1];
  #
  [3 7 2   0 0 0   5 0 0];
  [0 0 0   2 0 8   0 0 0];
  [0 0 8   0 0 0   1 7 2];
  #
  [8 0 0   0 0 5   4 1 7];
  [0 0 0   0 8 0   0 0 6];
  [7 5 0   6 4 3   0 8 0] ]
end

function sudoku2()::Array{Integer,2}
  [ [0 3 0   5 9 0   0 8 0];
  [0 0 2   0 0 6   9 7 0];
  [0 0 0   0 0 0   0 0 0];
  #
  [0 0 7   0 1 9   0 2 0];
  [0 0 3   0 0 0   1 0 0];
  [0 8 0   7 3 0   6 0 0];
  #
  [0 0 0   0 0 0   0 0 0];
  [0 2 8   9 0 0   4 0 0];
  [0 6 0   0 2 4   0 5 0] ]
end

function sudoku2X2()::Array{Integer,2}
  [ [1 0 ];
  [0 0 ] ]
end

function sudoku3X3()::Array{Integer,2}
  [ [1 0 0];
  [2 1 0];
  [0 0 1]
  ]
end

function sudoku4X4()::Array{Integer,2}
  [ [2 1 0 0];
  [0 3 2 0];
  [0 0 0 4];
  [1 0 0 0]
  ]
end

function sudoku4X4_multiple()::Array{Integer,2}
  [ [1 2 3 4];
  [3 4 0 0];
  [2 0 0 0];
  [4 0 0 0]
  ]
end
