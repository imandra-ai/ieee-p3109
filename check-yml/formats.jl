module Formats

  using Lerche

  @enum Signedness Signed=1 Unsigned=2

  @enum Domain Extended=1 Finite=2

  struct VariableRange
    from::Int
    to::Int
  end

  struct Format
    k::Union{Int,String}
    p::Union{Int,String}
    sigma::Signedness
    delta::Domain
    variable_ranges::Dict{String,VariableRange}
  end

  Base.show(io::IO, f::Format) = begin
    k = f.k isa String ? "{$(f.k)}" : f.k
    p = f.p isa String ? "{$(f.p)}" : f.p
    s = f.sigma == Signed ? "s" : "u"
    d = f.delta == Extended ? "e" : "f"
    b = "Binary$(k)p$(p)$(s)$(d)"
    for (name, vr) in f.variable_ranges
      b *= " for $(name)=$(vr.from)..$(vr.to)"
    end
    print(io, b)
  end

  grammar = raw"""
      start: expr

      format_spec: /(B|b)inary([0-9]+|{[a-z]+})p([0-9]+|{[a-z]+})(s|u)?(e|f)?/ -> format_spec

      expr:
        | "(" expr ")" -> par_expr
        | "[" expr ("," expr)* "]" -> format_list
        | format_spec ( "for" /[a-z]+/ "=" INT ".." INT )* -> format_expr

      %import common.INT
      %import common.CNAME
      %import common.WS

      %ignore WS
  """;

  struct FETransformer <: Transformer end

  @inline_rule format_spec(t::FETransformer, tkn) = begin
    m = Base.match(r"(B|b)inary(?<k>([0-9]+|{[a-z]+}))p(?<p>([0-9]+|{[a-z]+}))(?<sigma>(s|u)?)(?<delta>(e|f)?)", String(tkn))
    ["binary", m["k"], m["p"], m["sigma"] == "s" ? Signed : Unsigned, m["delta"] == "e" ? Extended : Finite]
  end

  @rule format_list(t::FETransformer, tkn) = begin
    fst = tkn[1]
    st = typeof(fst)
    new_type = Array{st}
    r = convert(new_type, tkn)
    return r
  end

  @rule par_expr(t::FETransformer, tkn) = tkn[1]

  @rule format_expr(t::FETransformer, tkn) = begin
    fspec = tkn[1]
    vars = Set()
    k = try
      Base.parse(Int, fspec[2])
    catch
      name = String(fspec[2][2:end-1])
      push!(vars, name)
      name
    end
    p = try
      Base.parse(Int, fspec[3])
    catch
      name = String(fspec[3][2:end-1])
      push!(vars, name)
      name
    end
    vinx = 2
    vranges = Dict{String,VariableRange}()
    while vinx < length(tkn)
      name::String = String(tkn[vinx])
      if !(name in vars)
        throw("unknown variable")
      end
      if haskey(vranges, name)
        throw("multiple assignments to variable")
      end
      delete!(vars, name)
      push!(vranges, name => VariableRange(Base.parse(Int, tkn[vinx + 1]), Base.parse(Int, tkn[vinx + 2])))
      vinx += 3
    end
    if length(vars) != 0
      throw("not all variables assigned")
    end
    Format(k, p, fspec[4], fspec[5], vranges)
  end

  function parse(s::String)
    p = Lark(grammar, parser="lalr", lexer="standard")
    t = Lerche.parse(p, s)
    r = Lerche.transform(FETransformer(), t)
    return r.children[1]
  end

  function unfold(f::Format)::Set{Format}
    r = Set{Format}()

    vranges = f.variable_ranges

    if typeof(f.k) == String
      vr = pop!(vranges, f.k)
      for i in vr.from:vr.to
        push!(r, Format(i, f.p, f.sigma, f.delta, vranges))
      end
    else
      push!(r, f)
    end

    if typeof(f.p) == String
      vr = pop!(vranges, f.p)
      r2 = Set{Format}()
      for f in r
        for i in vr.from:vr.to
          push!(r2, Format(f.k, i, f.sigma, f.delta, vranges))
        end
      end
      r = r2
    end

    r
  end

end