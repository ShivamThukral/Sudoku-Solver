include("dpll.jl")
import Base: !

struct SymBool
  leaf::Union{Bool, Literal}
  op::Symbol
  args::Tuple{Vararg{Any,N} where N}
  SymBool(leaf::Bool) = new(leaf, :none, ())
  SymBool(leaf::Literal) = new(leaf, :none, ())
  SymBool(leaf::Variable) = new(Literal(leaf), :none, ())
  SymBool(op::Symbol, args...) = new(false, op, args)
end

Base.show(io::IO, x::SymBool)::Nothing =
  if(x.op == :none) print(io, string("SymBool(", repr(x.leaf), ")"))
  else
    r = [string(", ", repr(a)) for a in x.args]
    print(io, string("SymBool(", repr(x.op), r..., ")"))
  end

Base.print(io::IO, x::SymBool)::Nothing =
  if(x.op == :none) print(io, x.leaf)
  else
    print(io, x.op, "(")
    if(length(x.args) >= 1)
      print(io, x.args[1])
      for a in x.args[2:length(x.args)]
	print(io, ", ", a)
      end
    end
    print(io, ")")
  end

SymBoolFix(x::Variable) = SymBool(x)
SymBoolFix(x::Literal) = SymBool(x)
SymBoolFix(x::Bool) = SymBool(x)
SymBoolFix(x::SymBool) = x

and(args...) = SymBool(:and, [SymBoolFix(x) for x in args]...)
or(args...) = SymBool(:or, [SymBoolFix(x) for x in args]...)
not(x::SymBool) = SymBool(:not, x)
!(x::SymBool) = not(x)
eq(x::SymBool, y::SymBool) = or(and(x,y), and(!x,!y))
eq(x::SymBool, y::Union{Variable,Literal,Bool}) = eq(x, SymBoolFix(y))
eq(x::Union{Variable,Literal,Bool}, y::SymBool) = eq(SymBoolFix(x), y)

function vars(x::SymBool, v::Set{Variable})::Set{Variable}
  if(typeof(x.leaf) == Literal)
    push!(v, x.leaf.var)
  else
    for y in x.args
      vars(y, v)
    end
  end
  return v
end
vars(x::SymBool)::Set{Variable} = vars(x, Set{Variable}([]))

function(x::SymBool)(m::Model)::MaybeBool
  a = [y(m) for y in x.args]
  if(x.op == :none)
    if(typeof(x.leaf) == Bool) x.leaf
    else x.leaf(m)
    end
  elseif(x.op == :and)
    if(reduce((p,q) -> p || (q == false), a; init=false))
      false
    elseif(reduce((p,q) -> p && (q == true), a; init=true))
      true
    else
      undef_bool
    end
  elseif(x.op == :or)
    if(reduce((p,q) -> p || (q == true), a; init=false))
      true
    elseif(reduce((p,q) -> p && (q == false), a; init=true))
      false
    else
      undef_bool
    end
  elseif(x.op == :not)
    !a[1]
  else
    error(string("unknown operator, ", x.op))
  end
end

struct ClausifyData
  vars::Set{Variable}
  top_lit::Union{Literal,Bool}
  next_free::Integer
  clauses::Array{Clause,1}
end

function fresh_var(vars::Set{Variable}, next_free::Integer)::Tuple{Variable,Integer}
  i = next_free
  v() = Variable("\$" * repr(i))
  while v() in vars
    i = i+1
  end
  vi = v()
  return (vi, i+1)
end

ClausifyMemo = Dict{SymBool,ClausifyData}

function clausify(x::SymBool, vars::Set{Variable}, next_free::Integer, mem::ClausifyMemo)::ClausifyData
  if(haskey(mem, x))
    mem_cd = mem[x]
    ClausifyData(vars, mem_cd.top_lit, next_free, [])
  elseif(x.op == :none) mem[x] = ClausifyData(vars, x.leaf, next_free, [])
  elseif((x.op == :and) || (x.op == :or))
    and_neg(p) = if(x.op == :and) !p else p end
    or_neg(p) = if(x.op == :or) !p else p end
    (my_var, next_free) = fresh_var(vars, next_free)
    my_lit = Literal(my_var)
    cd_new = ClausifyData(push!(vars, my_var), my_lit, next_free, Array{Clause,1}([]))
    clauses = [];
    cl1 = [or_neg(my_lit)]
    cl2 = Array{Clause,1}([])
    cl3 = Array{Array{Clause,1},1}([])
    for y in x.args
      cd_new = clausify(y, cd_new.vars, cd_new.next_free, mem)
      if(typeof(cd_new.top_lit) == Bool)
        if(or_neg(cd_new.top_lit)) continue
        else return mem[x] = ClausifyData(vars, cd_new.top_lit, next_free, Array{Clause,1}([]))
	end
      else
	push!(cl1, and_neg(cd_new.top_lit))
	push!(cl2, Clause([and_neg(my_lit), or_neg(cd_new.top_lit)]))
	push!(cl3, cd_new.clauses)
      end
    end
    mem[x] = ClausifyData(cd_new.vars, my_lit, cd_new.next_free, cat(dims=1, [Clause(cl1)], cl2, cl3...))
  elseif(x.op == :not)
    cd_new = clausify(x.args[1], vars, next_free, mem)
    mem[x] = ClausifyData(cd_new.vars, !cd_new.top_lit, cd_new.next_free, cd_new.clauses)
  end
end

function clausify(x::SymBool, vars::Set{Variable}, next_free::Integer)::ClausifyData
  clausify(x, vars, next_free, ClausifyMemo())
end

function clausify(x::SymBool)::ClausifyData
  clausify(x, vars(x), 0)
end


struct UnSAT end
unsat = UnSAT()
Base.print(io::IO, u::UnSAT)::Nothing = print(io, "unsat")

function solve(formula::SymBool)::Union{Model, UnSAT}
  v = vars(formula)
  clf = clausify(formula, v, 0)
  if(typeof(clf.top_lit) == Bool)  # trivial formula
    if(clf.top_lit) return Model()
    else return unsat
    end
  else
    s = sat(cnf(vcat(clf.clauses, [Clause(clf.top_lit)])))
    if(typeof(s) == Bool)
      if(s) error("I'm confused")
      else return unsat
      end
    else
      return Model(v => (haskey(s, v) ? s[v] : false) for v in vars(formula))
    end
  end
end

struct QED end
qed = QED()
Base.print(io::IO, q::QED)::Nothing = print(io, "\u25a2")

struct CEX m::Model end
Base.print(io::IO, cex::CEX)::Nothing = print(io, "Counter-example:\n", cex.m)

function(x::SymBool)(cex::CEX)::MaybeBool
  x(cex.m)
end

function prove(formula::SymBool)::Union{QED,CEX}
  s = solve(not(formula))
  if(s == unsat) return qed
  else return CEX(s)
  end
end
