include("symbool.jl");

x = Variable("x");
y = Variable("y");
z = Variable("z");

# compare two ways of calculating the majority of three boolean values
m1 = or(and(x,y), and(y,z), and(z,x));
m2 = and(or(x,y), or(y,z), or(z,x));

# m1 and m2 are of type SymBool
println("typeof(m1) = ", typeof(m1), ", typeof(m2) = ", typeof(m2));

# just for fun, we can evaluate a SymBool with a given model:
print("x=true,  y=true,  z=false: m1 = ", m1(Model(x=>true,  y=>true,  z=>false)),"\n");
print("x=false, y=true,  z=false: m1 = ", m1(Model(x=>false, y=>true,  z=>false)),"\n");

# if a variable is missing in the model, it's value is undefined.  The
# evaluator "knows" that and(false, undef) -> false, and
# or(true, undef) -> true.  Thus, we may be able to get a defined value
# for the SymBool even if with a partial model:
print("x=false, z=false: m1 = ", m1(Model(x=>false, z=>false)),"\n");

# but just knowing x is not enough to determine the value of m1(x,y,z):
print("x=false: m1 = ", m1(Model(x=>false)),"\n");


# let's prove that m1 and m2 are equivalent
#print(prove(eq(m1, m2)));
# It prints a box to indicate a successful proof.  Just like in a math text.  :)

# What happens if we try to prove something that's not a theorem?
m3 = and(or(x,y), or(y,z), or(z,!x));
print(prove(eq(m1, m3)));

# we can get the counter-example and "debug" the claim:
cex = prove(eq(m1,m3));
m1(cex)  # prints true
m3(cex)  # prints false
or(x,y)(cex)  # true
or(y,z)(cex)  # true
or(z,!x)(cex)  # false -- ah, this must be our problem

# remember that proving something is just the same as showing that it's
# negation is satisfiable.  So, we could try:
solve(!eq(m1,m3))
# and we get a model for the counter-example.  Sometimes, we want a
# formula to be satisfiable, and we're interested in the solution --
# for example, if we want to solve a Sudoku puzzle.  solve(x::Symbol)
# is great for this.
