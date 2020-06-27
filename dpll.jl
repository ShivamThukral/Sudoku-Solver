struct UndefBool end
undef_bool = UndefBool()     # "bottom" value for booleans, undefined value

Base.print(io::IO, u::UndefBool)::Nothing = print(io, "undef")

MaybeBool = Union{Bool, UndefBool}

import Base.xor

# I'm not sure what xor(top, bot) should be.
#   The code I wrote below makes xor(top, bot) = xor(bot, top) = top.
#   I could also see a reason to make it bot.  The challenge is that
#   In two-valued logic, we have:
#     xor(x, y) = or(and(x, not(y)), and(not(x), y))
#               = and(or(x, not(y)), or(not(x), y))
#   For the "obvious" extensions of and, or, and not to a four-valued lattice,
#   we get
#        or(and(top, not(bot)), and(not(top), bot))
#     =  or(and(top, top), and(bot, bot)
#     = or(top, bot)
#     = true
#   and
#     and(or(top, not(bot)), or(not(top), bot)) = false
#   If it turns out to matter, I'll need to dig deeper into some texts on logic.
function xor(x::MaybeBool, y::MaybeBool)::MaybeBool
  if((x == undef_bool) || (y == undef_bool))
    undef_bool
  else
    Base.xor(x, y)
  end
end

struct Variable
  name::String
end

Base.print(io::IO, v::Variable)::Nothing = print(io, v.name)

Model = Dict{Variable,MaybeBool}

function(v::Variable)(m::Model)::MaybeBool
  if haskey(m, v) m[v] else undef_bool end
end

struct Literal
  var::Variable
  negated::Bool
  Literal(v::Variable, neg::Bool) = new(v, neg)
  Literal(v::Variable) = new(v, false)
  Literal(k::Symbol, v::Variable) =
    if(k == :not) new(v, true)
    else error(string("Literal(k=", repr(k), ", v=", repr(v), "): k must be :not"))
    end
end

Base.show(io::IO, lit::Literal) =
  if(lit.negated) print(io, string("Literal(:not, ", repr(lit.var), ")"))
  else print(io, string("Literal(", repr(lit.var), ")"))
  end
Base.print(io::IO, lit::Literal)::Nothing =
  if(lit.negated) print(io, "!", lit.var)
  else print(io, lit.var)
  end

import Base.!
!(v::Variable) = Literal(:not, v)
!(lit::Literal) = Literal(lit.var, !lit.negated)
!(u::UndefBool) = undef_bool

function(lit::Literal)(m::Model)::MaybeBool
  xor(lit.negated, lit.var(m))
end

struct Clause
  lit::Array{Literal}
  Clause(a::Array{Literal}) = new(a)
  Clause(aa...) = new([
    if(typeof(x) == Literal) x else Literal(x) end
    for x in aa ])
  Clause(x::Array{Any}) = Clause(x...)
  Clause(x::Tuple{Vararg{Any}}) = Clause(x...)
end
      
Base.print(io::IO, cl::Clause)::Nothing = begin
  n = length(cl.lit)
  print(io, "(")
  for i = 1:n
    print(cl.lit[i])
    if (i < n) print(", ") end
  end
  print(io, ")")
end
 
function(cl::Clause)(m::Model)::Union{Clause,Bool}
  a = Array{Literal}(undef,0)
  for lit::Literal in cl.lit
    v = lit(m)
    if(v == true) return true
    elseif(v == undef_bool) push!(a, lit)
    end
  end
  if(length(a) == 0) false else Clause(a) end
end

struct CNF_Formula
  cl::Array{Clause}
  level::Integer
  CNF_Formula(a::Array{Clause}, level::Integer) = new(a, level)
  CNF_Formula(a::Array{Clause}) = new(a, 0)
end

Base.print(io::IO, c::CNF_Formula)::Nothing = begin
  n = length(c.cl)
  print(io, "[")
  for i = 1:n
    print(c.cl[i])
    if (i < n) print(",\n ") end
  end
  print(io, "]")
end

function could_become_a_clause(x)::Bool
  if(typeof(x) == Clause) true
  elseif((typeof(x) <: Array) || (typeof(x) <: Tuple))
    if(length(x) == 0) true
    else (typeof(x[1]) == Literal) || (typeof(x[1]) == Variable)
    end
  else false
  end
end

function as_clause(x)::Clause
  (typeof(x) == Clause) ? x : Clause(x)
end

function cnf(cl::Clause, level::Integer=0)::CNF_Formula
  CNF_Formula(Array{Clause,1}([cl]), level)
end
function cnf(cla::Array{Clause,1}, level::Integer=0)::CNF_Formula
  CNF_Formula(cla, level)
end
function cnf(a...)::CNF_Formula
  if(length(a) == 0)
    CNF_Formula(Array{Clause,1}([]))
  elseif(could_become_a_clause(a))
    CNF_Formula(Array{Clause,1}([as_clause(a)]))
  elseif(could_become_a_clause(a[1]))
    CNF_Formula(Array{Clause,1}([as_clause(cl) for cl in a]))
  elseif(    (length(a) == 1)
          && ((typeof(a[1]) <: Array) || (typeof(a[1]) <: Tuple))
          && could_become_a_clause(a[1][1]))
    CNF_Formula(Array{Clause,1}([as_clause(cl) for cl in a[1]]))
  else error(string("CNF_Formula(", repr(a...), "): I don't know how to make a CNF_Formula from that"))
  end
end
 
function(c::CNF_Formula)(m::Model, level::Integer = c.level)::Union{CNF_Formula,Bool}
  a = Array{Clause}(undef,0)
  for cl::Clause in c.cl
    v = cl(m)
    if(v == false) return(false)
    elseif(typeof(v) == Clause)
      push!(a,v)
    end
  end
  if(length(a) == 0) true else CNF_Formula(a, level) end
end

do_spew = false;

function spew(s::String, indent::Integer=0)
  if(do_spew)
    for i in 1:indent
      print(" ")
    end
    print(s)
    print("\n")
  end
end
function spew(s::String, c::CNF_Formula)
  spew(s, 2*c.level)
end


function vars(c::CNF_Formula)::Set{Variable}
  reduce(union!,
    (Set([lit.var for lit in cl.lit]) for cl in c.cl),
    init=Set(Variable[]))
end

function unit_propagate(c::CNF_Formula)::Tuple{Union{CNF_Formula,Bool},Model}
  m = Model()
  while(typeof(c) == CNF_Formula)
    mlen = length(m)
    u = filter(cl -> length(cl.lit) == 1, c.cl)
    if(length(u) == 0)
      break
    end
    for cl in u
      m[cl.lit[1].var] = ~(cl.lit[1].negated)
      # spew(string("unit_propagate: ", cl.lit[1].var.name, " => ",
      #             repr(~(cl.lit[1].negated))),
      #	     c)
    end
    c = c(m)
  end
  return c, m
end
function unit_propagate(c::Bool)::Tuple{Union{CNF_Formula,Bool},Model}
  c, Model()
end

function pure_eliminate(c::CNF_Formula)::Tuple{Union{CNF_Formula,Bool},Model}
  p = Model([(v, undef_bool) for v in vars(c)])
  for cl in c.cl
    for lit in cl.lit
      v = lit.var
      if(haskey(p, v))
	pval = v(p)          # value for v in p
	cval = !lit.negated  # value for v that satisfies cl
	if(pval == undef_bool)
	  p[lit.var] = cval;
	elseif(pval != cval) # v appears in different polarities in different clauses
	  delete!(p, v)
	end
      end
    end
  end
  # all remaining variables in p are pure
  for v in keys(p)
    if(typeof(p[v]) == Bool)
      spew(string("pure_eliminate: ", v.name, " => ", repr(p[v])), c)
    else
      error(string("p[", v.name, "] = ", repr(p[v]), "but it should be a boolean."))
    end
  end
  return c(p), p
end
function pure_eliminate(c::Bool)::Tuple{Bool,Model}
  c, Model()
end

function sat(c)::Union{Model,Bool}
  m = Model()
  mlen = -1  # check for unit clauses and pure variables at least once
  while(mlen < length(m))
    mlen = length(m)
    c, mu = unit_propagate(c)
    c, mp = pure_eliminate(c)
    merge!(m, mu, mp)
  end
  if(c == true) return(m)
  elseif(c == false) return(false)
  else
    # pick a variable to branch and recurse.
    # c must have at least one clause (otherwise c(m) == true)
    # and each clause must have at least one variable (otherwise c(m) == false)
    v = c.cl[1].lit[1].var
    for val in (true, false)
      spew(string("branch: ", v.name, " => ", repr(val)), c)
      mx = sat(c(Model(v => val), c.level+1))
      if(mx != false)
	m[v] = val
	return merge!(m, mx)
      end
    end
    return(false)
  end
end
