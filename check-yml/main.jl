import YAML
using Format
using Match
using Base.Iterators

include("formats.jl")

known_functions = [
  "Negate",
  "Abs",
  "CopySign",
  "Recip",
  "Sqrt",
  "Rsqrt",
  "Add",
  "Subtract",
  "Multiply",
  "Divide",
  "Hypot",
  "FMA",
  "FAA",
  "Exp",
  "Exp2",
  "Log",
  "Log2"]

function check_required_keys(section::String, d::Any, keys::Vector{String})::Bool
  if !(d isa Dict)
    printfmtln("Error: {} section should be a dictionary", section)
    return false
  end
  for key in keys
    if !haskey(d, key)
      printfmtln("Error: Missing required key `{}` in section `{}`", key, section)
      return false
    end
  end
  return true
end

function check_optional_keys(section::String, d::Any, optional_keys::Vector{String})::Bool
  if !(d isa Dict)
    printfmtln("Error: {} section should be a dictionary", section)
    return false
  end
  for key in keys(d)
    if findfirst(isequal(key), optional_keys) === nothing
      printfmtln("Error: Invalid optional key `{}` in section `{}`", key, section)
      return false
    end
  end
  return true
end

function print_operation(name, d::Dict{String,Any}, formats)
  for (k, v) in d
    for f in v
      @match f begin
        "Negate" | "Abs" => begin
          rho = "?"
          for f_x in formats
            for f_r in formats
              printfmtln("{}: {}[{},{},{}]", name, f, f_x, f_r, rho)
            end
          end
        end
        _ =>
          printfmtln("NIY: {}", f)
      end
    end
  end
end



function check_profiles(file)
  data = YAML.load_file(file, dicttype=Dict{String,Any})
  # println(data)
  m = "Metadata"
  md = data[m]

  if !check_required_keys(m, md, ["conformance", "version", "provider"]) ||
     !check_optional_keys(m, md, ["conformance", "version", "provider", "contact", "implementation", "Configurations", "Profiles"])
    return []
  end

  printfmtln("Profiles conformance: {}", md["conformance"])
  profiles = filter(e -> e != "Metadata", keys(data))
  format_exprs = []

  for p in profiles
    d = data[p]
    if !(d isa Vector)
      printfmtln("Error: Profile `{}` should be a vector", p)
      return []
    end
    d = d[1]
    if !haskey(d, "formats")
      printfmtln("Error: Profile `{}` does not define `formats`", p)
      return []
    end
    push!(format_exprs, d["formats"])
  end

  formats = Set{Formats.Format}()
  for expr in format_exprs
    if !(expr isa Vector)
      expr = [expr]
    end
    for e in expr
      # printfmtln("Parsing: `{}`", e)
      fp = Formats.parse(e)
      fs = fp isa Vector ? collect(flatten(fp)) : [fp]
      foreach(x -> push!(formats, x), collect(flatten(map(Formats.unfold, fs))))
    end
  end

  printfmtln("Profiles: {}", profiles)
  printfmtln("Formats: {}", formats)
  return profiles, formats
end


function check_std_operations(file, profiles, formats)
  data = YAML.load_file(file, dicttype=Dict{String,Any})
  # println(data)

  m = "Metadata"
  md = data[m]
  if !check_required_keys(m, md, ["description", "reference", "source"])
    return false
  end

  printfmtln("Standard operations: {}", md["description"])
  for (k, v) in data
    @match k begin
      "Metadata" => continue
      "Comparison" => continue
      "Extrema" => continue
      "Projection" => continue
      "Block" => continue
      "Conversion" => continue
      "Classification" => continue
      "Math" => begin
        check_optional_keys(k, v, ["sign", "arithmetic", "transcendental"])
        print_operation("whatsmyname?", v, formats)
      end
      "Format" => continue
      _ => begin
        printfmtln("Error: Unknown operation block `{}`", k)
        return false
      end
    end
  end

  return true
end

(@main)(args) = begin
  if length(args) != 1
    throw("invalid number of arguments")
  end
  dir = args[1]
  profiles, formats = check_profiles(joinpath(dir, "Profiles.yaml"))
  check_std_operations(joinpath(dir, "StandardOperations.yaml"), profiles, formats)
end
