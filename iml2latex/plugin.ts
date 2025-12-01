import { Doc, doc, AST, AstPath, Options } from "prettier";
import camelize from 'camelize-ts'

const { group, indent, dedent, join, ifBreak, breakParent, line, hardline, softline, fill } = doc.builders;

const iml2json = require('./iml2json.bc').iml2json;
import { assert } from 'node:console';

var PREFIX = "Cer";
var function_names: string[] | undefined = undefined;

function fun_filter(name: string, options: Options) {
  if (globalThis.function_names)
    return globalThis.function_names.includes(name);
  else
    return name.startsWith("w") && name == "internal_wSaturate";
};


export const languages = [
  {
    extensions: ['.iml'],
    name: 'IML',
    parsers: ['iml2latex-parse']
  }
];

interface Tree {
  top_defs: string[][];
  comments: string[];
}

export const parsers = {
  'iml2latex-parse': {
    parse: (text: string, options: Options): Tree => {
      try {
        const jdefs = iml2json.parse(text);
        // console.log(jdefs);
        const x: Tree = {
          top_defs: JSON.parse(jdefs),
          comments: []
        };
        // console.log(x.top_defs);
        return x;
      } catch (e) {
        // If parsing fails for any reason, we just throw a generic error to abort formatting.
        console.log(e);
        throw new Error("Parser error");
      }
    },
    astFormat: 'iml2latex-ast',
    hasPragma: (_text: string): boolean => { return false; },
    locStart: (_node: Tree): number => { return 0; },
    locEnd: (_node: Tree): number => { return 0; },
    preprocess: (text: string, _options: Options): string => { return text; },
  }
};

export const printers = {
  'iml2latex-ast': { print }
};

function id2latex(id: string) {
  if (id == "NaN") { return ["\\NaN"]; }
  else if (id == "PINF") { return ["+\\infty"]; }
  else if (id == "NINF") { return ["-\\infty"]; }
  else if (id == "true") { return ["\\True"]; }
  else if (id == "false") { return ["\\False"]; }
  else if (
    ["Extended", "Finite",
      "Signed", "Unsigned",
      "TowardZero",
      "TowardPositive", "TowardNegative",
      "NearestTiesToEven", "NearestTiesToAway"].includes(id)
  ) {
    return ["\\" + id];
  }
  if (id.includes("_")) {
    id = id.replace("_", "_{");
    id = "{" + id + "}}";
  }
  return id;
}

function is_pm_inf(a0, a1) {
  return a0.ppat_desc[0] == "Ppat_construct" && a1.ppat_desc[0] == "Ppat_construct"
    && (a0.ppat_desc[1].txt[1] == "PINF" && a1.ppat_desc[1].txt[1] == "NINF");
}

function is_mp_inf(a0, a1) {
  return a0.ppat_desc[0] == "Ppat_construct" && a1.ppat_desc[0] == "Ppat_construct"
    && (a0.ppat_desc[1].txt[1] == "NINF" && a1.ppat_desc[1].txt[1] == "PINF");
}

function check_undef(x) {
  if (!(x instanceof Array))
    assert(false)
  else
    x.forEach(e => {
      assert(e !== undefined && e !== null);
      if (e instanceof Array)
        check_undef(e);
      else
        if (e instanceof Object)
          assert((e as object).hasOwnProperty("type"));
    });
}

function g(x: Doc[]) {
  check_undef(x);
  return group(x);
}

function f(x: Doc[]) {
  check_undef(x);
  return fill(x);
}

function doc_to_string(x: Doc): string {
  if (x === undefined)
    return "UNDEFINED"
  else if (x === null)
    return "NULL"
  else if (x instanceof Array)
    return "[" + x.map(e => doc_to_string(e)).join(" ") + "]";
  else if (x instanceof Object && x.hasOwnProperty("type")) {
    switch (x.type) {
      case "group": return "G(" + doc_to_string(x.contents) + ")";
      case "fill": return "F(" + doc_to_string(x.parts) + ")";
      case "line": return "L"
      case "indent": return "I(" + doc_to_string(x.contents) + ")"
      case "break-parent": return "BP"
      default:
        return `UNKNOWN TYPE ${x.type}`
    }
  }
  else if (typeof (x) == "string")
    return x;
  else
    return `NIY`;
}

function niy() {
  throw new Error("not implemented yet");
}

function get_source(start, end, options: Options): string {
  const from = start.loc_start.pos_cnum;
  const to = end.loc_end.pos_cnum;
  return (options.originalText as string).slice(from, to);
}

function ifnonempty(x, d: Doc): Doc[] {
  if (!d)
    return [d];
  else if (d instanceof Array && d.length == 0)
    return d;
  else
    return [x, d] as Doc[];
}

enum Notation { None, Infix, Prefix, Outfix }
enum Associativity { None, Left, Right }

class PrecedenceInfo {
  name: string;
  notation: Notation;
  associativity: Associativity;
  precedence: number;

  constructor(name: string,
    notation: Notation,
    associativity: Associativity,
    precedence: number) {
    this.name = name;
    this.notation = notation;
    this.associativity = associativity;
    this.precedence = precedence;
  }
}

function operator_precedence_info(op: string | undefined, more_than_one_arg = false): PrecedenceInfo {
  // https://ocaml.org/manual/5.3/expr.html#ss:precedence-and-associativity

  if (op !== undefined) {

    // Not sure ~- is handled correctly here.

    // prefix-symbol
    if ((op.startsWith("!") || op.startsWith("?") || op.startsWith("~")) && op.length > 1 && op != "~-" && op != "~-.")
      return new PrecedenceInfo(op, Notation.Prefix, Associativity.None, 20);
    // . .( .[ .{ (see section 12.11)
    if (op.startsWith("#"))
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Left, 18);
    // function application, constructor application, tag application, assert, lazy
    if (op == "assert" || op == "lazy") // rest at the bottom.
      return new PrecedenceInfo(op, Notation.Prefix, Associativity.Left, 17);
    // - -. (prefix)
    if ((op == "-" || op == "-." || op == "~-" || op == "~-.") && !more_than_one_arg)
      return new PrecedenceInfo(op, Notation.Prefix, Associativity.None, 16);
    // **… lsl lsr asr
    if (op.startsWith("**") || op == "lsl" || op == "lsr" || op == "asr")
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Right, 15);
    // *… /… %… mod land lor lxor
    if (op.startsWith("*") || op.startsWith("/") || op.startsWith("%") || op == "mod" || op == "land" || op == "lor" || op == "lxor")
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Left, 14);
    // +… -…
    if (op.startsWith("+") || op.startsWith("-"))
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Left, 13);
    // ::
    if (op == "::")
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Right, 12);
    // @… ^…
    if (op.startsWith("@") || op.startsWith("^"))
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Right, 11);
    // =… <… >… |… &… $… !=
    if (op.startsWith("=") || op.startsWith("<") ||
      op.startsWith(">") ||
      (op.startsWith("|") && op != "||") ||
      (op.startsWith("&") && op != "&" && op != "&&") ||
      op.startsWith("$") || op == "!=")
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Left, 10);
    // & &&
    if (op == "&" || op == "&&")
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Right, 9);
    // or ||
    if (op == "or" || op == "||")
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Right, 8);
    // ,	–
    if (op == ",")
      return new PrecedenceInfo(op, Notation.None, Associativity.None, 7);
    // <- :=
    if (op == "<-" || op == ":=")
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Right, 6);
    // if
    if (op == "if")
      return new PrecedenceInfo(op, Notation.None, Associativity.None, 5);
    // ;
    if (op == ";")
      return new PrecedenceInfo(op, Notation.Infix, Associativity.Right, 4);
    // let match fun function try
    if (op == "let" || op == "match" || op == "fun" || op == "function" || op == "try")
      return new PrecedenceInfo(op, Notation.None, Associativity.None, 3);

    if (op == "implies" || op == "==>")
      return new PrecedenceInfo("==>", Notation.Infix, Associativity.Right, 8.3);
    if (op == "explies" || op == "<==")
      return new PrecedenceInfo("<==", Notation.Infix, Associativity.Left, 8.2);
    if (op == "iff" || op == "<==>")
      return new PrecedenceInfo("<==>", Notation.Infix, Associativity.None, 8.1);
  }

  if (op == "Real.abs")
    return new PrecedenceInfo("|", Notation.Outfix, Associativity.None, 17);
  else if (op == "Real.min")
    return new PrecedenceInfo("\\Min", Notation.Prefix, Associativity.Left, 17);
  else if (op == "Real.max")
    return new PrecedenceInfo("\\Max", Notation.Prefix, Associativity.Left, 17);
  else if (op == "Util.pow2")
    return new PrecedenceInfo("2^", Notation.Prefix, Associativity.None, 15);
  else if (op == "Util.reciprocal")
    return new PrecedenceInfo("1/", Notation.Prefix, Associativity.None, 12);
  else if (op == "Exp.exp")
    return new PrecedenceInfo("e^", Notation.Prefix, Associativity.None, 14);
  else if (op == "Log.log")
    return new PrecedenceInfo("log_e", Notation.Prefix, Associativity.Left, 17);
  else if (op == "Log.log2")
    return new PrecedenceInfo("log_2", Notation.Prefix, Associativity.Left, 17);
  else if (op == "Pi.pi")
    return new PrecedenceInfo("\\pi", Notation.Prefix, Associativity.Left, 17);
  else if (op == "sin" || op == "cos" || op == "tan" || op == "sinh" || op == "cosh" || op == "tanh")
    return new PrecedenceInfo(op, Notation.Prefix, Associativity.Left, 17);
  else if (op == "Util.ripow")
    return new PrecedenceInfo("^", Notation.Infix, Associativity.None, 13);
  else if (op == "Sqrt.sqrt")
    return new PrecedenceInfo("\\sqrt", Notation.Prefix, Associativity.Left, 12);

  // function application, constructor application, tag application
  return new PrecedenceInfo(op, Notation.Prefix, Associativity.Left, 17);
}

function operator_precedence(op: string): number {
  return operator_precedence_info(op).precedence;
}

function apply_precedence(): number {
  return operator_precedence("assert");
}

function op_info_of_expr(expr): PrecedenceInfo {
  if (
    expr.pexp_desc[0] == "Pexp_apply" &&
    expr.pexp_desc[1].pexp_desc[0] == "Pexp_ident") {
    const op_ident2 = longident2string(expr.pexp_desc[1].pexp_desc[1].txt);
    return operator_precedence_info(op_ident2, expr.pexp_desc[2].length > 1);
  }
  else switch (expr.pexp_desc[0]) {
    case "Pexp_lazy": return operator_precedence_info("lazy");
    case "Pexp_assert": return operator_precedence_info("assert");
    case "Pexp_ifthenelse": return operator_precedence_info("if");
    case "Pexp_let": return operator_precedence_info("let");
    case "Pexp_match": return operator_precedence_info("match");
    case "Pexp_function": return operator_precedence_info("fun");
    case "Pexp_try": return operator_precedence_info("try");
    case "Pexp_tuple": return operator_precedence_info(",");
    default: return operator_precedence_info(undefined);
  }
};

function op_info_of_pat(p): PrecedenceInfo {
  switch (p.ppat_desc[0]) {
    case "Ppat_lazy": return operator_precedence_info("lazy");
    case "Ppat_tuple": return operator_precedence_info(",");
    default: return operator_precedence_info(undefined);
  }
};

function print_longident(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args: AST[] = node.slice(1);
  switch (constructor) {
    case "Lident":
      // | Lident of string
      return [id2latex(args[0])];
    case "Ldot":
      // | Ldot of t * string
      return [print_longident(args[0], options), ".", softline, args[1]];
    case "Lapply":
      // | Lapply of t * t
      niy();
      break;
  }
}

function longident2string(node: AST): string {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Lident":
      // | Lident of string
      return args[0] as string;
    case "Ldot":
      // | Ldot of t * string
      return longident2string(args[0]) + "." + args[1];
    case "Lapply":
      // | Lapply of t * t
      niy();
      break;
  }
}

function print_longident_loc(node: AST, options: Options): Doc {
  return print_longident(node.txt, options);
}

function par_if(c: boolean, x: Doc): Doc[] {
  const needs_space = x instanceof Array && typeof (x[0]) == "string" && x[0].startsWith("*");
  const l = needs_space ? line : softline;
  return c ? ["(", l, x, l, ")"] : [x];
}

function print_constant_desc(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Pconst_integer": {
      // 	| Pconst_integer of string * char option
      // 	(** Integer constants such as [3] [3l] [3L] [3n].

      //  Suffixes [[g-z][G-Z]] are accepted by the parser.
      //  Suffixes except ['l'], ['L'] and ['n'] are rejected by the typechecker
      // *)
      const val = 0; // args[0];
      const r = [args[0]];
      if (args[1])
        r.push(args[1]);
      return f(par_if(val < 0, r));
    }
    case "Pconst_char":
      // | Pconst_char of char  (** Character such as ['c']. *)
      return ["'", args[0], "'"];
    case "Pconst_string": {
      // | Pconst_string of string * Location.t * string option
      // 	(** Constant string such as ["constant"] or
      // 			[{delim|other constant|delim}].

      //  The location span the content of the string, without the delimiters.
      // *)
      const delim = args[2] ? args[2] : "\"";
      return [delim, args[0], delim];
    }
    case "Pconst_float": {
      // | Pconst_float of string * char option
      // 	(** Float constant such as [3.4], [2e5] or [1.4e-4].

      //  Suffixes [g-z][G-Z] are accepted by the parser.
      //  Suffixes are rejected by the typechecker.
      // *)
      if (args[0] == "0.0") return "\\zero";
      if (args[0] == "1.0") return "\\one";
      if (args[0] == "-1.0") return "-\\one";
      const val = 0; // args[0];
      return par_if(val < 0, args[1] ? args[0].concat(args[1]) : args[0]);
    }
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_constant(node: AST, options: Options): Doc {
  // {
  //   pconst_desc : constant_desc;
  //   pconst_loc : Location.t;
  // }
  return g([print_constant_desc(node.pconst_desc, options)]);
}

function print_payload(node: AST, options: Options): Doc[] {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "PStr": {
      // | PStr of structure
      return print_structure(args[0], options);
    }
    case "PSig":
    case "PTyp":
    case "PPat":
      return [];
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_module_expr_desc(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Pmod_ident":
      // | Pmod_ident of Longident.t loc  (** [X] *)
      return print_longident_loc(args[0], options);
    case "Pmod_structure":
      // | Pmod_structure of structure  (** [struct ... end] *)
      // return f([indent(["struct", hardline,
      //   join([hardline, hardline], print_structure(args[0], options))]), hardline,
      //   "end"]);
      return f(print_structure(args[0], options));
    case "Pmod_functor":
    case "Pmod_apply":
    case "Pmod_apply_unit":
    case "Pmod_constraint":
    case "Pmod_unpack":
    case "Pmod_extension":
      return [];
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_string_loc(node: AST, options: Options): string {
  return node.txt;
}

const attribute_filter = [
  "iml.semisemi",
  "imandra_verify", "imandra_instance", "imandra_theorem",
  "imandra_eval", "imandra_axiom", "imandra_rule_spec",
  "ocaml.text"
];

function filter_attributes(attrs: AST[]): AST[] {
  return attrs.filter(x => !attribute_filter.find(y => y == x.attr_name.txt));
}

function print_attributes(attrs: AST[], level: number, options: Options): Doc[] {
  const filtered = filter_attributes(attrs);
  return join(line, filtered.map(x => print_attribute(x, level, options)));
}

function has_attribute(attrs, x): boolean {
  return attrs.find(a => a.attr_name.txt == x);
}

function has_attribute_with_payload(attrs, x, payload): boolean {
  return attrs.find(a =>
    a.attr_name.txt == x &&
    a.attr_payload[0] == "PStr" &&
    a.attr_payload[1][0].pstr_desc[0] == "Pstr_eval" &&
    a.attr_payload[1][0].pstr_desc[1].pexp_desc[0] == "Pexp_constant" &&
    a.attr_payload[1][0].pstr_desc[1].pexp_desc[1].pconst_desc[1] == payload);
}

function print_arg_label(node: AST, options: Options, with_tilde = true): Doc[] {
  // type arg_label =
  //   Nolabel
  // | Labelled of string (** [label:T -> ...] *)
  // | Optional of string (** [?label:T -> ...] *)
  const constructor = node instanceof Array && node.length > 0 ? node[0] : node;
  switch (constructor) {
    case "Nolabel":
      return [];
    case "Labelled":
      return [...(with_tilde ? ["~"] : []), print_label(node[1], options), ":", softline];
    case "Optional":
      return ["?", print_label(node[1], options), ":", softline];
    default:
      throw new Error(`invalid arg_label: ${node}`);
  }
}

function print_core_type_desc(node: AST, options: Options): Doc[] {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Ptyp_any":
      // | Ptyp_any  (** [_] *)
      return ["_"];
    case "Ptyp_var":
      // | Ptyp_var of string  (** A type variable such as ['a] *)
      return ["'", args[0]] as Doc[];
    case "Ptyp_arrow":
      // | Ptyp_arrow of arg_label * core_type * core_type
      //     (** [Ptyp_arrow(lbl, T1, T2)] represents:
      //           - [T1 -> T2]    when [lbl] is
      //                                    {{!Asttypes.arg_label.Nolabel}[Nolabel]},
      //           - [~l:T1 -> T2] when [lbl] is
      //                                    {{!Asttypes.arg_label.Labelled}[Labelled]},
      //           - [?l:T1 -> T2] when [lbl] is
      //                                    {{!Asttypes.arg_label.Optional}[Optional]}.
      //        *)
      return [
        print_arg_label(args[0], options, false), // Apparently OCaml doesn't put a tilde here.
        print_core_type(args[1], options),
        line, "->", line,
        print_core_type(args[2], options)];
    case "Ptyp_tuple":
      // | Ptyp_tuple of core_type list
      //     (** [Ptyp_tuple([T1 ; ... ; Tn])]
      //         represents a product type [T1 * ... * Tn].

      //          Invariant: [n >= 2].
      //       *)
      return [join([line, "*", line], args[0].map(x => print_core_type(x, options)))];
    case "Ptyp_constr": {
      // | Ptyp_constr of Longident.t loc * core_type list
      //     (** [Ptyp_constr(lident, l)] represents:
      //           - [tconstr]               when [l=[]],
      //           - [T tconstr]             when [l=[T]],
      //           - [(T1, ..., Tn) tconstr] when [l=[T1 ; ... ; Tn]].
      //        *)
      let r: Doc[] = [];
      if (args[1].length > 1)
        r.push("(");
      r = r.concat(join([",", line], args[1].map(x => print_core_type(x, options))));
      if (args[1].length > 1)
        r.push(")");
      if (args[1].length > 0)
        r.push(line);
      return r.concat(print_longident_loc(args[0], options));
    }
    case "Ptyp_alias":
      return [];
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_core_type(node: AST, options: Options): Doc[] {
  // {
  //  ptyp_desc: core_type_desc;
  //  ptyp_loc: Location.t;
  //  ptyp_loc_stack: location_stack;
  //  ptyp_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
  // }
  return [
    ...print_core_type_desc(node.ptyp_desc, options),
    ...ifnonempty(line, print_attributes(node.ptyp_attributes, 1, options))];
}

function print_label(node: AST, options: Options): Doc {
  // type label = string
  return node;
}

function capitalize_first(s: string) {
  if (s.startsWith("{"))
    return s[0] + s[1].toUpperCase() + s.slice(2);
  else
    return s.replace(/^./, s[0].toUpperCase());
}


function print_pattern_desc(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Ppat_any":
      // | Ppat_any  (** The pattern [_]. *)
      return "\\any";
    case "Ppat_var": {
      // | Ppat_var of string loc  (** A variable pattern such as [x] *)
      let r;
      if (((args[0].txt[0] != 's' &&
        options.hasOwnProperty("pattern_reals") &&
        options.pattern_reals instanceof Array &&
        options.pattern_reals.includes(args[0].txt)))
        || options.move_everything_up)
        r = capitalize_first(print_string_loc(args[0], options));
      else
        r = print_string_loc(args[0], options);
      if (r.includes("_")) {
        if (options.move_everything_up)
          r = r.replace("_", "^");
        else
          r = r.replace("_", "_{") + "}";
      }
      return r;
    }
    case "Ppat_alias":
      // | Ppat_alias of pattern * string loc
      //     (** An alias pattern such as [P as 'a] *)
      return f([print_pattern(args[0], options), line, "as", line, print_string_loc(args[1], options)]);
    case "Ppat_constant":
      // | Ppat_constant of constant
      //     (** Patterns such as [1], ['a'], ["true"], [1.0], [1l], [1L], [1n] *)
      return print_constant(args[0], options);
    case "Ppat_interval":
      // | Ppat_interval of constant * constant
      //     (** Patterns such as ['a'..'z'].
      //          Other forms of interval are recognized by the parser
      //          but rejected by the type-checker. *)
      return f([print_constant(args[0], options), "..", print_constant(args[1], options)]);
    case "Ppat_tuple":
      // | Ppat_tuple of pattern list
      //     (** Patterns [(P1, ..., Pn)].
      //          Invariant: [n >= 2]
      //       *)
      return f([join([",", line], args[0].map(x => print_pattern(x, options)))]);
    case "Ppat_construct":
      // | Ppat_construct of Longident.t loc * (string loc list * pattern) option
      //     (** [Ppat_construct(C, args)] represents:
      //           - [C]               when [args] is [None],
      //           - [C P]             when [args] is [Some ([], P)]
      //           - [C (P1, ..., Pn)] when [args] is
      //                                          [Some ([], Ppat_tuple [P1; ...; Pn])]
      //           - [C (type a b) P]  when [args] is [Some ([a; b], P)]
      //        *)
      if (args[1] instanceof Array && args[1].length > 0) {
        const op_ident = longident2string(args[0].txt);
        const op_info = operator_precedence_info(op_ident);
        if (op_ident == "R") {
          if (args[1][1].ppat_desc[0] != 'Ppat_var' && args[1][1].ppat_desc[0] != 'Ppat_constant')
            return ["{\\color{red}REAL", print_pattern(args[1][1], options), "}"];
          else {
            if (args[1][1].ppat_desc[0] == 'Ppat_var' && options.pattern_reals instanceof Array)
              options.pattern_reals.push(args[1][1].ppat_desc[1].txt);
            let r = [print_pattern(args[1][1], options)];
            return r;
          }
        }
        else if (op_info.notation == Notation.Infix) {
          assert(args[1][1].ppat_desc[0] == "Ppat_tuple");
          const [l, r] = args[1][1].ppat_desc[1];
          if (op_info.name == "::")
            return print_pattern_list(args[1][1], options);
          else
            return [print_pattern(l, options), line, op_info.name, line, print_pattern(r, options)];
        }
        else {
          let cargs = join([";", line], args[1][0].map(sl => print_string_loc(sl, options)));
          if (cargs.length > 0)
            cargs = ["(type", line, cargs, softline, ")"];
          let r = [print_longident_loc(args[0], options), line,
          print_pattern(args[1][1], options)];
          if (cargs && cargs.length > 0)
            r = r.concat([line, cargs]);
          return f(r);
        }
      }
      else {
        if (args[0].txt instanceof Array && args[0].txt.length == 2) {
          if (args[0].txt[1] == "SatFinite" ||
            args[0].txt[1] == "SatPropagate" ||
            args[0].txt[1] == "OvfInf")
            return "\\" + args[0].txt[1];
        }
        return print_longident_loc(args[0], options);
      }
    case "Ppat_variant":
      // | Ppat_variant of label * pattern option
      //     (** [Ppat_variant(`A, pat)] represents:
      //           - [`A]   when [pat] is [None],
      //           - [`A P] when [pat] is [Some P]
      //        *)
      return f(["`", print_label(args[0], options), ...ifnonempty(line, print_pattern(args[1], options))]);
    case "Ppat_record": {
      // | Ppat_record of (Longident.t loc * pattern) list * closed_flag
      //     (** [Ppat_record([(l1, P1) ; ... ; (ln, Pn)], flag)] represents:
      //           - [{ l1=P1; ...; ln=Pn }]
      //                when [flag] is {{!Asttypes.closed_flag.Closed}[Closed]}
      //           - [{ l1=P1; ...; ln=Pn; _}]
      //                when [flag] is {{!Asttypes.closed_flag.Open}[Open]}

      //          Invariant: [n > 0]
      //        *)
      const r = join([";", line], args[0].map(x => f([print_longident_loc(x[0], options), line, "=", line, print_pattern(x[1], options)])));
      if (!args[1] || args[1] == "Open")
        r.push([";", line, "_"]);
      return ["{", line, ...r, line, "}"];
    }
    case "Ppat_array":
      // | Ppat_array of pattern list  (** Pattern [[| P1; ...; Pn |]] *)
      return f(["[|", line, join([";", line], args[0].map(x => print_pattern(x, options))), line, "|]"]);
    case "Ppat_or":
      // | Ppat_or of pattern * pattern  (** Pattern [P1 | P2] *)
      if (is_pm_inf(args[0], args[1]))
        return "\\pm\\infty";
      else if (is_mp_inf(args[0], args[1]))
        return "\\mp\\infty";
      else
        return f([print_pattern(args[0], options), line, "\\vee", line, print_pattern(args[1], options)]);
    case "Ppat_constraint":
      // | Ppat_constraint of pattern * core_type  (** Pattern [(P : T)] *)
      // return f(["(", print_pattern(args[0], options), line, ":", line, print_core_type(args[1], options), ")"]);
      return f([print_pattern(args[0], options)]);
    case "Ppat_type":
    case "Ppat_lazy":
    case "Ppat_unpack":
    case "Ppat_exception":
    case "Ppat_effect":
    case "Ppat_open":
    case "Ppat_extension":
      return [];
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_pattern(node: AST, options: Options): Doc {
  // pattern =
  //   {
  //    ppat_desc: pattern_desc;
  //    ppat_loc: Location.t;
  //    ppat_loc_stack: location_stack;
  //    ppat_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
  //   }
  return g([
    f([
      print_pattern_desc(node.ppat_desc, options),
      ...ifnonempty(line, print_attributes(node.ppat_attributes, 1, options))])
  ]);
}

function print_value_constraint(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Pvc_constraint":
      // | Pvc_constraint of { locally_abstract_univars:string loc list; typ:core_type; }
      return g([
        ...ifnonempty(line, args[0].locally_abstract_univars.map(print_string_loc)),
        print_core_type(args[0].typ, options)]);
    case "Pvc_coercion":
      return [];
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_value_binding(node: AST, options: Options): Doc {
  // {
  //   pvb_pat: pattern;
  //   pvb_expr: expression;
  //   pvb_constraint: value_constraint option;
  //   pvb_attributes: attributes;
  //   pvb_loc: Location.t;
  // }(** [let pat : type_constraint = exp] *)
  return g([
    f([print_pattern(node.pvb_pat, options),
    ...(node.pvb_constraint ? ifnonempty([line, ":", line], print_value_constraint(node.pvb_constraint, options)) : []),
      line, "=", line,
    ...print_expression(node.pvb_expr, options)])]);
}

function print_expression(node: AST, options: Options): Doc[] {
  // {
  // 	pexp_desc: expression_desc;
  // 	pexp_loc: Location.t;
  // 	pexp_loc_stack: location_stack;
  // 	pexp_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
  //  }
  return [
    print_expression_desc(node.pexp_desc, options),
    ...ifnonempty(line, print_attributes(node.pexp_attributes, 1, options))];
}

function print_function_param_desc(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Pparam_val": {
      // | Pparam_val of arg_label * expression option * pattern
      // (** [Pparam_val (lbl, exp0, P)] represents the parameter:
      //     - [P]
      //       when [lbl] is {{!Asttypes.arg_label.Nolabel}[Nolabel]}
      //       and [exp0] is [None]
      //     - [~l:P]
      //       when [lbl] is {{!Asttypes.arg_label.Labelled}[Labelled l]}
      //       and [exp0] is [None]
      //     - [?l:P]
      //       when [lbl] is {{!Asttypes.arg_label.Optional}[Optional l]}
      //       and [exp0] is [None]
      //     - [?l:(P = E0)]
      //       when [lbl] is {{!Asttypes.arg_label.Optional}[Optional l]}
      //       and [exp0] is [Some E0]

      //     Note: If [E0] is provided, only
      //     {{!Asttypes.arg_label.Optional}[Optional]} is allowed.
      // *)
      const op_info_arg = op_info_of_pat(args[2]);
      const is_lower = op_info_arg.precedence < apply_precedence();
      switch (args[0][0]) {
        case "Nolabel": {
          return par_if(is_lower, print_pattern(args[2], options));
        }
        case "Labelled":
          return f(["~", par_if(is_lower, print_pattern(args[2], options))]);
        case "Optional": {
          const exp0 = args[1];
          if (!exp0)
            return f(["?", par_if(is_lower, print_pattern(args[2], options))]);
          else
            return f(["?",
              "(", par_if(is_lower, print_pattern(args[2], options)),
              line, "=", line,
              ...print_expression(exp0, options), ")"]);
        }
        default:
          return print_pattern(args[2], options);
      }
    }
    case "Pparam_newtype":
      return []
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_function_param(node: AST, options: Options): Doc {
  return f([
    print_function_param_desc(node.pparam_desc, options)
  ]);
}

function print_function_body(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Pfunction_body":
      //   | Pfunction_body of expression
      return f(print_expression(args[0], options));
    case "Pfunction_cases":
      //   | Pfunction_cases of case list * Location.t * attributes
      //   (** In [Pfunction_cases (_, loc, attrs)], the location extends from the
      //       start of the [function] keyword to the end of the last case. The compiler
      //       will only use typechecking-related attributes from [attrs], e.g. enabling
      //       or disabling a warning.
      //   *)
      // (** See the comment on {{!expression_desc.Pexp_function}[Pexp_function]}. *)
      return g([
        ...(constructor == "Pfunction_cases" ? ["function", breakParent, line] : []),
        ifBreak("| ", ""),
        ...join([line, "| "], args[0].map(c => print_case(c, options))),
        ...ifnonempty(line, print_attributes(args[2], 1, options))]);
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_module_expr_open_infos(node: AST, options: Options, with_prefix: boolean): Doc[] {
  // {
  //  popen_expr: 'a;
  //  popen_override: override_flag;
  //  popen_loc: Location.t;
  //  popen_attributes: attributes;
  // }
  let pre: Doc[] = [];
  if (with_prefix) {
    if (node.popen_override == "Override")
      pre = ["open!", line];
    else
      pre = ["open", line];
  }
  return [...pre,
  print_module_expr(node.popen_expr, options),
  ...ifnonempty(line, node.popen_attributes)];
}

function print_open_declaration(node: AST, options: Options, with_prefix: boolean): Doc[] {
  // 	open_declaration = module_expr open_infos
  // (** Values of type [open_declaration] represents:
  //     - [open M.N]
  //     - [open M(N).O]
  //     - [open struct ... end] *)
  return print_module_expr_open_infos(node, options, with_prefix);
}

function print_flat_list_elems(e: AST, options: Options): Doc[] {
  const d0 = print_expression(e.pexp_desc[1][0], options);
  const e1 = e.pexp_desc[1][1];
  if (e1.pexp_desc[0] == "Pexp_construct" && e1.pexp_desc[1].txt[1] == "[]")
    return d0;
  else if (e1.pexp_desc[0] == "Pexp_construct" && e1.pexp_desc[1].txt[1] == "::")
    return [...d0, ...print_flat_list_elems(e1.pexp_desc[2], options)];
  else
    return [...d0, ...print_expression(e1, options)];
}

function print_list(e: AST, options: Options): Doc {
  const es = print_flat_list_elems(e, options);
  return f([join([softline, "::", softline], es)]);
}

function print_flat_list_pattern_elems(p: AST, options: Options): Doc[] {
  const d0 = print_pattern(p.ppat_desc[1][0], options);
  const e1 = p.ppat_desc[1][1];
  if (e1.ppat_desc[0] == "Ppat_construct" && e1.ppat_desc[1].txt[1] == "[]")
    return [d0];
  else if (e1.ppat_desc[0] == "Ppat_construct" && e1.ppat_desc[1].txt[1] == "::")
    return [d0, ...print_flat_list_pattern_elems(e1.ppat_desc[2][1], options)];
  else
    return [d0, print_pattern(e1, options)];
}

function print_pattern_list(p: AST, options: Options): Doc {
  const ps = print_flat_list_pattern_elems(p, options);
  return f([join([softline, "::", softline], ps)]);
}

function print_case(node: AST, options: Options): Doc {
  // {
  // 	pc_lhs: pattern;
  // 	pc_guard: expression option;
  // 	pc_rhs: expression;
  // }
  let r = [print_pattern(node.pc_lhs, options)];
  if (node.pc_guard)
    r = r.concat(["when", line, print_expression(node.pc_guard, options)]);
  r = r.concat([
    line, "->", line,
    ...print_expression(node.pc_rhs, options)
  ]);
  return f(r);
}

function print_binding_op(node: AST, options: Options): Doc {
  // {
  //   pbop_op : string loc;
  //   pbop_pat : pattern;
  //   pbop_exp : expression;
  //   pbop_loc : Location.t;
  // }
  return g([
    f([
      print_string_loc(node.pbop_op, options), line,
      print_pattern(node.pbop_pat, options), line,
      "=", line,
      ...print_expression(node.pbop_exp, options)])]);
}

function print_letop(node: AST, options: Options): Doc {
  // {
  //   let_ : binding_op;
  //   ands : binding_op list;
  //   body : expression;
  // }
  const ands = join([line, "and"], node.ands.map(x => print_binding_op(x, options)));
  return f([
    print_binding_op(node.let_, options),
    ifnonempty(line, ands),
    line, "in", line,
    ...print_expression(node.body, options)]);
}

function is_apply_with_args(x: AST): boolean {
  return x.pexp_desc[0] == "Pexp_apply" && x.pexp_desc[2].length > 0;
}

function is_construct_with_args(x: AST): boolean {
  return x.pexp_desc[0] == "Pexp_construct" && x.pexp_desc[2] !== null;
}

function is_neg_const(x: AST): boolean {
  return x.pexp_desc[0] == "Pexp_constant" &&
    ((x.pexp_desc[1].pconst_desc[0] == "Pconst_float" ||
      x.pexp_desc[1].pconst_desc[0] == "Pconst_integer")
      &&
      Number(x.pexp_desc[1].pconst_desc[1]) < 0);
}

function is_infix_op(x: AST): boolean {
  const is_id = x.pexp_desc[0] == "Pexp_ident";
  if (is_id) {
    const op = longident2string(x.pexp_desc[1].txt);
    const op_info = operator_precedence_info(op);
    return op_info.notation == Notation.Infix;
  }
  return false;
}

function is_infix_op_pattern(x: AST): boolean {
  const is_var = x.ppat_desc[0] == "Ppat_var";
  if (is_var) {
    const op = x.ppat_desc[1].txt;
    const op_info = operator_precedence_info(op);
    return op_info.notation == Notation.Infix;
  }
  return false;
}

function print_expression_desc(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);

  switch (constructor) {
    case "Pexp_ident": {
      // | Pexp_ident of Longident.t loc
      //     (** Identifiers such as [x] and [M.x]
      //        *)
      let r;
      if (args[0].txt instanceof Array && args[0].txt.length == 2) {
        if (args[0].txt[1] == "sigma")
          return "\\sigma";
        if (args[0].txt[1] == "delta")
          return "\\Delta";
      }
      if (
        (options.hasOwnProperty("is_real") && options.is_real) ||
        (options.hasOwnProperty("pattern_reals") &&
          options.pattern_reals instanceof Array &&
          options.pattern_reals.includes(args[0].txt[1])) ||
        options.move_everything_up) {
        let n = print_longident_loc(args[0], options)[0];
        r = [(n.startsWith('s') || n.startsWith('{s')) ? // 's.*' are scaling factors
          n : capitalize_first(n)];
      }
      else
        r = print_longident_loc(args[0], options);
      if (options.move_everything_up && r[0].includes("_"))
        return r[0].replace("_", "^");
      else
        return r;
    }
    case "Pexp_constant":
      // | Pexp_constant of constant
      //     (** Expressions constant such as [1], ['a'], ["true"], [1.0], [1l],
      //           [1L], [1n] *)
      return print_constant(args[0], options);
    case "Pexp_let": {
      // | Pexp_let of rec_flag * value_binding list * expression
      //     (** [Pexp_let(flag, [(P1,E1) ; ... ; (Pn,En)], E)] represents:
      //           - [let P1 = E1 and ... and Pn = EN in E]
      //              when [flag] is {{!Asttypes.rec_flag.Nonrecursive}[Nonrecursive]},
      //           - [let rec P1 = E1 and ... and Pn = EN in E]
      //              when [flag] is {{!Asttypes.rec_flag.Recursive}[Recursive]}.
      //        *)
      const rec_flag = args[0];
      const value_bindings = args[1];
      const expr = args[2];
      const r: Doc[] = [];
      if (rec_flag instanceof Array && rec_flag[0] == "Recursive") {
        r.push(" rec");
      }
      return f(
        [...r, line,

        join([hardline, "and"], value_bindings.map(vb =>
          ["\\ensuremath{", print_value_binding(vb, options), "}"]
        )),
          line, hardline,
          ...print_expression(expr, options),
        ]);
    }
    case "Pexp_function":
      // | Pexp_function of
      //     function_param list * type_constraint option * function_body
      // (** [Pexp_function ([P1; ...; Pn], C, body)] represents any construct
      //     involving [fun] or [function], including:
      //     - [fun P1 ... Pn -> E]
      //       when [body = Pfunction_body E]
      //     - [fun P1 ... Pn -> function p1 -> e1 | ... | pm -> em]
      //       when [body = Pfunction_cases [ p1 -> e1; ...; pm -> em ]]

      //     [C] represents a type constraint or coercion placed immediately before the
      //     arrow, e.g. [fun P1 ... Pn : ty -> ...] when [C = Some (Pconstraint ty)].

      //     A function must have parameters. [Pexp_function (params, _, body)] must
      //     have non-empty [params] or a [Pfunction_cases _] body.
      // *)
      return f([
        "fun", line,
        ...join(line, args[0].map(n => print_function_param(n, options))), line, "->",
        indent([line, print_function_body(args[2], options)])]);
    case "Pexp_apply": {
      // | Pexp_apply of expression * (arg_label * expression) list
      //     (** [Pexp_apply(E0, [(l1, E1) ; ... ; (ln, En)])]
      //           represents [E0 ~l1:E1 ... ~ln:En]

      //           [li] can be
      //             {{!Asttypes.arg_label.Nolabel}[Nolabel]}   (non labeled argument),
      //             {{!Asttypes.arg_label.Labelled}[Labelled]} (labelled arguments) or
      //             {{!Asttypes.arg_label.Optional}[Optional]} (optional argument).

      //          Invariant: [n > 0]
      //        *)
      const op_expr = args[0];
      const op_args = args[1];
      let op_info: PrecedenceInfo;

      if (op_expr.pexp_desc[0] == "Pexp_ident") {
        const op_ident = longident2string(op_expr.pexp_desc[1].txt);
        op_info = operator_precedence_info(op_ident, op_args.length > 1);
      }
      else {
        op_info = new PrecedenceInfo(undefined, Notation.None, Associativity.None, apply_precedence());
      }

      op_info.name = op_info.name.replace("&&", "\\And");
      op_info.name = op_info.name.replace("||", "\\Or");
      op_info.name = op_info.name.replace("*.", "\\times");
      op_info.name = op_info.name.replace("Trigonometric.cos", "cos");

      if (op_info.name.startsWith("w")) {
        op_info.name = "\\" + PREFIX + "\\" + op_info.name.substring(1);
      }

      switch (op_info.notation) {
        case Notation.Infix: {
          assert(op_info.name !== undefined && op_args.length <= 2);
          let r: Doc[] = [];
          if (op_args.length > 0) {
            const op_info_left = op_info_of_expr(op_args[0][1]);
            r = [
              ...print_arg_label(op_args[0][0], options),
              ...par_if(
                op_info_left.precedence < op_info.precedence ||
                (op_info_left.precedence == op_info.precedence && op_info.associativity == Associativity.Right),
                print_expression(op_args[0][1], options))];
          }
          let opname = op_info.name;
          if ((opname.length == 2 || opname.length == 3) && opname[opname.length - 1] == ".")
            opname = opname.substring(0, opname.length - 1);
          if (opname == "<=") opname = "\\leq"
          else if (opname == ">=") opname = "\\geq"
          else if (opname == "<>") opname = "\\neq"
          r = r.concat([line, opname, line]);
          if (op_args.length > 1) {
            const op_info_right = op_info_of_expr(op_args[1][1]);
            r = r.concat([
              ...print_arg_label(op_args[1][0], options),
              ...par_if(
                // op_info_right.precedence < op_info.precedence ||
                // (op_info_right.precedence == op_info.precedence && op_info.associativity == Associativity.Left),
                false,
                print_expression(op_args[1][1], options))]);
          }
          return f(r);
        }
        case Notation.Prefix: {
          assert(op_info.name !== undefined);
          const r: Doc[] = join([", ", line], op_args.map(arg => {
            const op_info_arg = op_info_of_expr(arg[1]);
            return [
              ...print_arg_label(arg[0], options),
              ...par_if(
                op_info_arg.precedence < op_info.precedence ||
                (op_info_arg.precedence == op_info.precedence &&
                  (/* is_apply_with_args(arg[1]) || */ is_construct_with_args(arg[1]) || is_neg_const(arg[1]) || is_infix_op(arg[1]))),
                print_expression(arg[1], options))];
          }));
          let opname = op_info.name;
          if (opname == "-.") opname = "-";
          else if (opname == "~-.") opname = "-";
          // Drop the approximation precision from some operators
          if ([
            "e^", "\\sqrt",
            "log_e", "log_2", "Exp.exp2", "Log.ln",
            "sin", "cos", "tan",
            "sinh", "cosh", "tanh",
            "arcsin", "arccos", "arctan",
            "arcsinh", "arccosh", "arctanh",
            "\\pi"
          ].indexOf(opname) >= 0
          ) { r.pop(); r.pop(); }
          let want_par = false;
          if (opname == "Exp.exp2")
            opname = "2^";
          else if (opname == "Log.ln")
            opname = "log_e";
          if ([
            "log_e", "log_2",
            "sin", "cos", "tan",
            "sinh", "cosh", "tanh",
            "arcsin", "arccos", "arctan",
            "arcsinh", "arccosh", "arctanh"
          ].indexOf(opname) >= 0) {
            opname = "\\" + opname;
            want_par = false;
          }
          return f([opname, "{", par_if(r.length > 1 || want_par, [indent([line, ...r])]), "}"]);
        }
        case Notation.Outfix: {
          const r: Doc[] = join(line, op_args.map(arg => {
            // const op_info_arg = op_info_of_expr(arg[1]);
            return [print_expression(arg[1], options)];
          }));
          return f([op_info.name, indent([line, ...r]), op_info.name]);
        }
        case Notation.None: {
          return f([
            ...print_expression(op_expr, options), line,
            join(line, op_args.map(arg => {
              const op_info_arg = op_info_of_expr(arg[1]);
              return f([indent([
                ...print_arg_label(arg[0], options),
                ...par_if(
                  op_info_arg.precedence <= op_info.precedence,
                  print_expression(arg[1], options))])]);
            })),
          ]);
        }
        default:
          {
            throw new Error(`unknown operator notation '${op_info.notation as string}'`)
          }
      }
    }
    case "Pexp_match": {
      // | Pexp_match of expression * case list
      //     (** [match E0 with P1 -> E1 | ... | Pn -> En] *)
      const cs = join([line, "| "], args[1].map(arg => {
        const op_info_arg = op_info_of_expr(arg.pc_rhs);
        return f([
          print_pattern(arg.pc_lhs, options),
          line, "->", line,
          ...par_if(op_info_arg.precedence <= operator_precedence("match"),
            print_expression(arg.pc_rhs, options))]);
      }));
      return g([
        f(["match", indent([line, ...print_expression(args[0], options), line]), "with"]),
        line, ifBreak("| ", ""), ...cs]);
    }
    case "Pexp_try":
      // | Pexp_try of expression * case list
      //     (** [try E0 with P1 -> E1 | ... | Pn -> En] *)
      return g([
        "try",
        indent([line, ...print_expression(args[0], options), line]),
        line, "with", line,
        join([line, "| "], args[1].map(x => {
          return f([print_case(x, options)]);
        }))]);
    case "Pexp_tuple":
      // | Pexp_tuple of expression list
      //     (** Expressions [(E1, ..., En)]

      //          Invariant: [n >= 2]
      //       *)
      return f([join([",", line], args[0].map(x => print_expression(x, options)))]);
    case "Pexp_construct": {
      // | Pexp_construct of Longident.t loc * expression option
      // (** [Pexp_construct(C, exp)] represents:
      //      - [C]               when [exp] is [None],
      //      - [C E]             when [exp] is [Some E],
      //      - [C (E1, ..., En)] when [exp] is [Some (Pexp_tuple[E1;...;En])]
      //   *)
      const id = print_longident_loc(args[0], options);

      if (id == "R") {
        options.is_real = true;
        let r = [print_expression(args[1], options)];
        options.is_real = false;
        return r;
      }
      else if (id == "Ok") {
        let r = [];
        if (args[1]) {
          // const op_info = operator_precedence_info(undefined);
          // const op_info_arg = op_info_of_expr(args[1]);
          r = r.concat([
            line,
            // ...par_if(
            //   op_info_arg.precedence < op_info.precedence ||
            //   (op_info_arg.precedence == op_info.precedence &&
            //     (is_apply_with_args(args[1]) || /* is_construct_with_args(args[1]) || */ is_neg_const(args[1]))),
            print_expression(args[1], options)]);
        }
        return f(r);
      }
      else if (id == "::" && args[1] && args[1].pexp_desc[0] == "Pexp_tuple") {
        return print_list(args[1], options);
      }
      else {
        let r = [id];
        if (args[1]) {
          const op_info = operator_precedence_info(undefined);
          const op_info_arg = op_info_of_expr(args[1]);
          r = r.concat([
            line,
            ...par_if(
              op_info_arg.precedence < op_info.precedence ||
              (op_info_arg.precedence == op_info.precedence &&
                (is_apply_with_args(args[1]) || is_construct_with_args(args[1]) || is_neg_const(args[1]))),
              print_expression(args[1], options))]);
        }
        return f(r);
      }
    }
    case "Pexp_variant":
      // | Pexp_variant of label * expression option
      //     (** [Pexp_variant(`A, exp)] represents
      //           - [`A]   when [exp] is [None]
      //           - [`A E] when [exp] is [Some E]
      //        *)
      return f([print_label(args[0], options), ...ifnonempty(line, print_expression(args[1], options))]);
    case "Pexp_record": {
      // | Pexp_record of (Longident.t loc * expression) list * expression option
      //     (** [Pexp_record([(l1,P1) ; ... ; (ln,Pn)], exp0)] represents
      //           - [{ l1=P1; ...; ln=Pn }]         when [exp0] is [None]
      //           - [{ E0 with l1=P1; ...; ln=Pn }] when [exp0] is [Some E0]

      //          Invariant: [n > 0]
      //        *)
      const fields = join([";", line], args[0].map(id_expr => {
        const id = id_expr[0];
        const expr = id_expr[1];
        return f([print_longident_loc(id, options), line, "=", line, ...print_expression(expr, options)]);
      }));
      if (!args[1])
        return g(["{", indent([line, fields]), ";", line, "}"]);
      else
        return g(["{", indent([line, ...print_expression(args[1], options), line, "with", indent([line, fields])]), ";", line, "}"]);
    }
    case "Pexp_field":
      // | Pexp_field of expression * Longident.t loc  (** [E.l] *)
      return f([...print_expression(args[0], options), ".", softline, print_longident_loc(args[1], options)]);
    case "Pexp_setfield":
      // | Pexp_setfield of expression * Longident.t loc * expression
      //     (** [E1.l <- E2] *)
      return f([
        ...print_expression(args[0], options), ".", softline, print_longident_loc(args[1], options),
        line, "<-", line,
        ...print_expression(args[2], options)]);
    case "Pexp_array":
      // | Pexp_array of expression list  (** [[| E1; ...; En |]] *)
      return f(["[|", join([";", line], args[0].map(x => print_expression(x, options))), "|]"]);
    case "Pexp_ifthenelse":
      // | Pexp_ifthenelse of expression * expression * expression option
      //     (** [if E1 then E2 else E3] *)
      return g(["\\If",
        indent(f([line, ...print_expression(args[0], options), line])),
        "\\Then",
        indent(g([line, ...print_expression(args[1], options), line])),
        "\\Else",
        indent(g([line, ...print_expression(args[2], options)]))]);
    case "Pexp_sequence":
      // | Pexp_sequence of expression * expression  (** [E1; E2] *)
      return [...print_expression(args[0], options), ";", line, print_expression(args[1], options)];
    case "Pexp_while":
      // | Pexp_while of expression * expression  (** [while E1 do E2 done] *)
      return f([
        "while", line,
        ...print_expression(args[0], options), line,
        "do", hardline,
        ...print_expression(args[1], options), line,
        "done"]);
    case "Pexp_for":
      // | Pexp_for of pattern * expression * expression * direction_flag * expression
      //     (** [Pexp_for(i, E1, E2, direction, E3)] represents:
      //           - [for i = E1 to E2 do E3 done]
      //                when [direction] is {{!Asttypes.direction_flag.Upto}[Upto]}
      //           - [for i = E1 downto E2 do E3 done]
      //                when [direction] is {{!Asttypes.direction_flag.Downto}[Downto]}
      //        *)
      return f(["for", line,
        print_pattern(args[0], options), line, "=", line,
        ...print_expression(args[1], options), line,
        args[3] == "Upto" ? "to" : "downto", line,
        ...print_expression(args[2], options), line, "do", line,
        ...print_expression(args[4], options), line, "done"]);
    case "Pexp_constraint":
      // | Pexp_constraint of expression * core_type  (** [(E : T)] *)
      return f(["(",
        ...print_expression(args[0], options), line,
        ":", line,
        print_core_type(args[1], options), softline,
        ")"]);
    case "Pexp_coerce":
      // | Pexp_coerce of expression * core_type option * core_type
      //     (** [Pexp_coerce(E, from, T)] represents
      //           - [(E :> T)]      when [from] is [None],
      //           - [(E : T0 :> T)] when [from] is [Some T0].
      //        *)
      if (!args[1])
        return f(["(",
          ...print_expression(args[0], options), line,
          ":>", line,
          print_core_type(args[2], options), softline,
          ")"]);
      else
        return f(["(",
          ...print_expression(args[0], options), line,
          ":", line,
          print_core_type(args[1], options), line,
          ":>", line,
          print_core_type(args[2], options), softline,
          ")"]);
    case "Pexp_send":
    case "Pexp_new":
    case "Pexp_setinstvar":
    case "Pexp_override":
    case "Pexp_letmodule":
    case "Pexp_letexception":
    case "Pexp_assert":
    case "Pexp_lazy":
    case "Pexp_poly":
    case "Pexp_object":
    case "Pexp_newtype":
    case "Pexp_pack":
      return [];
    case "Pexp_open":
      // | Pexp_open of open_declaration * expression
      //     (** - [M.(E)]
      //           - [let open M in E]
      //           - [let open! M in E] *)
      const src = get_source(args[0].popen_loc, args[0].popen_loc, options);
      if (src.startsWith("open")) {
        // return f([
        //   ...par_if(true, ["let", line,
        //     ...print_open_declaration(args[0], options, true), line, "in", hardline,
        //     ...print_expression(args[1], options)])]);
        return print_expression(args[1], options);
      }
      else
        return f([
          ...par_if(true, [
            ...print_open_declaration(args[0], options, false), ".(", softline,
            ...print_expression(args[1], options)]), softline, ")"]);
    case "Pexp_letop":
      // | Pexp_letop of letop
      //     (** - [let* P = E0 in E1]
      //           - [let* P0 = E00 and* P1 = E01 in E1] *)
      return print_letop(args[0], options);
    case "Pexp_extension":
    case "Pexp_unreachable":
      return [];
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_constructor_arguments(node: AST, options: Options): Doc[] {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Pcstr_tuple":
      // | Pcstr_tuple of core_type list
      return [join([line, "*", line], args[0].map(x => print_core_type(x, options)))];
    case "Pcstr_record":
      // | Pcstr_record of label_declaration list
      //     (** Values of type {!constructor_declaration}
      //   represents the constructor arguments of:
      // - [C of T1 * ... * Tn]     when [res = None],
      //                             and [args = Pcstr_tuple [T1; ... ; Tn]],
      // - [C: T0]                  when [res = Some T0],
      //                             and [args = Pcstr_tuple []],
      // - [C: T1 * ... * Tn -> T0] when [res = Some T0],
      //                             and [args = Pcstr_tuple [T1; ... ; Tn]],
      // - [C of {...}]             when [res = None],
      //                             and [args = Pcstr_record [...]],
      // - [C: {...} -> T0]         when [res = Some T0],
      //                             and [args = Pcstr_record [...]].
      return ["{", indent([line, join([";", line], args[0].map(x => print_label_declaration(x, options))), ";"]), line, "}"];
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_constructor_declaration(node: AST, options: Options): Doc {
  // {
  //  pcd_name: string loc;
  //  pcd_vars: string loc list;
  //  pcd_args: constructor_arguments;
  //  pcd_res: core_type option;
  //  pcd_loc: Location.t;
  //  pcd_attributes: attributes;  (** [C of ... [\@id1] [\@id2]] *)
  // }
  let r: Doc = [];
  if (node.pcd_args[1].length == 0)
    r = [node.pcd_name.txt];
  else
    r = [
      print_string_loc(node.pcd_name, options), line, "of", line,
      print_constructor_arguments(node.pcd_args, options)];
  return [...r, ...ifnonempty(line, print_attributes(node.pcd_attributes, 1, options))]
}

function print_label_declaration(node: AST, options: Options): Doc {
  // {
  // 	pld_name: string loc;
  // 	pld_mutable: mutable_flag;
  // 	pld_type: core_type;
  // 	pld_loc: Location.t;
  // 	pld_attributes: attributes;  (** [l : T [\@id1] [\@id2]] *)
  //  }
  // TODO: mutable
  return g([
    f([
      print_string_loc(node.pld_name, options), line, ":", line,
      print_core_type(node.pld_type, options),
      ...ifnonempty(line, print_attributes(node.pld_attributes, 1, options))])]);
}

function print_type_kind(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Ptype_abstract":
      // | Ptype_abstract
      return [];
    case "Ptype_variant":
      // | Ptype_variant of constructor_declaration list
      return g([ifBreak("| ", ""), join([line, "| "], args[0].map(x =>
        f([print_constructor_declaration(x, options)])
      ))]);
    case "Ptype_record":
      // | Ptype_record of label_declaration list  (** Invariant: non-empty list *)
      return g([
        "{",
        line,
        join([";", line], args[0].map(x => print_label_declaration(x, options))),
        ";",
        dedent([line, "}"])]);
    case "Ptype_open":
      // | Ptype_open
      niy();
      break;
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print_type_declaration(node: AST, options: Options): Doc {
  // {
  // 	ptype_name: string loc;
  // 	ptype_params: (core_type * (variance * injectivity)) list;
  // 	 (** [('a1,...'an) t] *)
  // 	ptype_cstrs: (core_type * core_type * Location.t) list;
  // 	 (** [... constraint T1=T1'  ... constraint Tn=Tn'] *)
  // 	ptype_kind: type_kind;
  // 	ptype_private: private_flag;  (** for [= private ...] *)
  // 	ptype_manifest: core_type option;  (** represents [= T] *)
  // 	ptype_attributes: attributes;  (** [... [\@\@id1] [\@\@id2]] *)
  // 	ptype_loc: Location.t;
  //  }

  return g([
    f([
      print_string_loc(node.ptype_name, options), " ",
      ...ifnonempty(["= ", ifBreak(line)], print_type_kind(node.ptype_kind, options)),
      ...ifnonempty(["= ", ifBreak(line)], (node.ptype_manifest ? [print_core_type(node.ptype_manifest, options)] : [])),
      ...ifnonempty(line, print_attributes(node.ptype_attributes, 2, options))])]); // TODO: rest
}

function print_module_expr(node: AST, options: Options): Doc {
  // {
  // 	pmod_desc: module_expr_desc;
  // 	pmod_loc: Location.t;
  // 	pmod_attributes: attributes;  (** [... [\@id1] [\@id2]] *)
  //  }
  return [print_module_expr_desc(node.pmod_desc, options)];
}

function print_module_binding(node: AST, options: Options): Doc {
  // {
  // 	pmb_name: string option loc;
  // 	pmb_expr: module_expr;
  // 	pmb_attributes: attributes;
  // 	pmb_loc: Location.t;
  //  }
  return [print_module_expr(node.pmb_expr, options)];
}

function get_attr_payload_string(node: AST): Doc {
  // Comments have special string payloads without quotes. Sigh.
  return node.attr_payload[1][0].pstr_desc[1].pexp_desc[1].pconst_desc[1];
}

function print_attribute(node: AST, level: number, options: Options): Doc[] {
  // {
  //   attr_name : string loc;
  //   attr_payload : payload;
  //   attr_loc : Location.t;
  // }
  switch (level) {
    case 3: {
      switch (node.attr_name.txt) {
        case "ocaml.text": {
          if (node.attr_payload[0] != "Pstr") {
            const str = get_attr_payload_string(node);
            return ["(**", indent(str), "*)"];
          }
          else
            return ["(**", ...print_payload(node.attr_payload, options), "*)"];
        }
        case "import": {
          const expr = node.attr_payload[1][0].pstr_desc[1];
          const is_pair = expr.pexp_desc[0] == "Pexp_tuple" && expr.pexp_desc[1].length == 2;
          let r: Doc[] = [];
          if (is_pair) {
            // We want a tuple without parentheses in this case.
            r = [
              ...print_expression(expr.pexp_desc[1][0], options), ",", line,
              ...print_expression(expr.pexp_desc[1][1], options),
            ];
          }
          else
            r = print_payload(node.attr_payload, options);
          return [f(["[@@@", print_string_loc(node.attr_name, options), line, ...r, "]"])];
        }
        case "iml.semisemi":
          return [";;"]
      }
      break;
    }
    default: {
      switch (node.attr_name.txt) {
        case "ocaml.doc": {
          const str = get_attr_payload_string(node);
          return ["(", "*".repeat(level), indent(str), "*)"];
        }
        case "ocaml.text": {
          const str = get_attr_payload_string(node);
          return [, "(*", str, "*)"];
        }
      }
    }
  }
  const payload = print_payload(node.attr_payload, options);
  // console.log(doc_to_string(payload));
  return [f(["[", "@".repeat(level),
    print_string_loc(node.attr_name, options),
    indent([...ifnonempty(line, payload), "]"])])];
}

function strip_error_handling(node: AST) {
  if (node.pexp_desc[0] == "Pexp_match" && node.pexp_desc[2][0].pc_lhs.ppat_desc[1].txt[1] == "Ok")
    return node.pexp_desc[1];
  return node;
}


function print_structure_item_desc(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Pstr_eval": {
      // | Pstr_eval of expression * attributes  (** [E] *)
      let r: Doc[] = [];
      if (args.length > 1 && has_attribute(args[1], "imandra_eval"))
        r = ["eval", line, "(", softline, ...print_expression(args[0], options), ")"];
      else
        r = [...print_expression(args[0], options)];
      return g([...r, ...ifnonempty(line, print_attributes(args[1], 3, options))]);
    }
    case "Pstr_value": {
      // | Pstr_value of rec_flag * value_binding list
      // 		(** [Pstr_value(rec, [(P1, E1 ; ... ; (Pn, En))])] represents:
      // 					- [let P1 = E1 and ... and Pn = EN]
      // 							when [rec] is {{!Asttypes.rec_flag.Nonrecursive}[Nonrecursive]},
      // 					- [let rec P1 = E1 and ... and Pn = EN ]
      // 							when [rec] is {{!Asttypes.rec_flag.Recursive}[Recursive]}.
      // 			*)
      const r: Doc[] = [];
      let is_instance_or_verify = false;
      const pvb = args[1][0];
      let attrs = pvb.pvb_attributes;

      if (attrs.length > 0) {
        if (has_attribute(attrs, "imandra_theorem")) {
          // Could be a theorem or a lemma; search backwards for the keyword.
          const cloc = args[1][0].pvb_loc;
          const src = options.originalText as string;
          let from = cloc.loc_start.pos_cnum - 1;
          const whitespace_chars = [" ", "\t", "\n", "\r"];
          while (from > 0 && whitespace_chars.find(x => x == src[from])) {
            from--;
          }
          from--;
          while (from > 0 && !whitespace_chars.find(x => x == src[from])) {
            from--;
          }
          if (from >= 0 && src.slice(from + 1, from + 6) == "lemma")
            r.push("lemma");
          else
            r.push("theorem");
        }
        else if (has_attribute(attrs, "imandra_instance")) {
          r.push("instance");
          is_instance_or_verify = true;
        }
        else if (has_attribute(attrs, "imandra_verify")) {
          r.push("verify");
          is_instance_or_verify = true;
        }
        else
          r.push("let");
      }
      else
        r.push("let");
      if (args[0] instanceof Array && args[0][0] == "Recursive") {
        r.push(" rec");
      }
      attrs = filter_attributes(attrs);
      if (is_instance_or_verify) {
        return [f([
          ...r,
          indent([
            line, "(", softline,
            ...print_expression(pvb.pvb_expr, options),
            softline, ")"]),
          ...ifnonempty(line, print_attributes(attrs, 2, options))])];
      }
      else if (args[1].length > 0 && pvb.pvb_expr.pexp_desc[0] == "Pexp_function") {
        let fname = pvb.pvb_pat.ppat_desc[1].txt;
        if (fun_filter(fname, options)) {
          let r = ["\\noindent" as Doc];

          fname = camelize(fname);
          if (/\d/.test(fname)) {
            // Latex commands can't have digits in their name; needs \\csname.
            fname = fname.replace(/^w/, "");
            fname = `\\${PREFIX}\\csname ${fname}\\endcsname `;
          }
          else
            fname = fname.replace(/^w/, "\\" + PREFIX + "\\");
          fname = fname.replace(/Fma/, "FMA");
          fname = fname.replace(/Faa/, "FAA");
          fname = fname.replace(/Rsqrt/, "RSqrt");
          fname = fname.replace(/internal(.*)/, "\\$1");
          fname = fname.replaceAll("_", "\_");
          const params = pvb.pvb_expr.pexp_desc[1];
          let fundef = pvb.pvb_expr.pexp_desc[3];
          fundef = fundef[1];
          while (fundef.pexp_desc[0] == "Pexp_open")
            fundef = fundef.pexp_desc[2];

          if (fname == "\\WSaturate") {
            fname = "\\" + PREFIX + "\\Saturate"
            options.move_everything_up = true;
          }

          if (fname == "\\Cer\\MaximumNumber" || fname == "\\Cer\\MinimumNumber")
            options.move_everything_up = true;

          while (fundef.pexp_desc[0] == "Pexp_let") {
            r = r.concat(join([hardline], fundef.pexp_desc[2].map(vb =>
              ["\\Case{", print_value_binding(vb, options), "}", "\\\\"]
            )));
            r.push(hardline);
            fundef = fundef.pexp_desc[3];
          }

          if (fundef.pexp_desc[0] == "Pexp_match") {
            for (let i = 0; i < fundef.pexp_desc[2].length; i++) {
              const arg = fundef.pexp_desc[2][i];
              if (arg.pc_lhs.ppat_desc[0] == "Ppat_any") {
                continue;
              }
              let rhs_expr = arg.pc_rhs;
              rhs_expr = strip_error_handling(rhs_expr);
              let pat = [fname, "(",
                print_pattern(arg.pc_lhs, options),
                ")", line,
              ];
              r = r.concat(f([
                "\\Case{",
                pat,
                ...(arg.pc_guard ?
                  ["~\\If", line, print_expression(arg.pc_guard, options)] : []),
                "\\gives", line,
                (has_attribute_with_payload(rhs_expr.pexp_attributes, "ocaml.text", "breakindent") ?
                  ["\\\\\\phantom{\\qquad", pat, "}"] : []),
                print_expression(rhs_expr, options), "}\\\\", hardline]));
              options.pattern_reals = [];
            }
            options.move_everything_up = false;
            return f(r);
          }
          else
            // return [...r,
            // f([indent([
            //   line,
            //   ...par_if(is_infix_op_pattern(pvb.pvb_pat), print_pattern(pvb.pvb_pat, options)),
            //   line,
            //   ...join(line, params.map(x => print_function_param(x, options))),
            //   ...(params && params.length > 0 ? [line] : []),
            //   "="]),
            //   f([indent([line, print_expression(fundef, options)]),
            // ...ifnonempty(line, print_attributes(attrs, 2, options))])])];
            return print_expression(fundef, options);
        }
        else
          return [];
      }
      return [];
    }
    case "Pstr_primitive":
    case "Pstr_type":
    case "Pstr_typext":
    case "Pstr_exception":
      return [];
    case "Pstr_module":
      // | Pstr_module of module_binding  (** [module X = ME] *)
      // return f(["module", line, args[0].pmb_name.txt, line, "=", line, print_module_binding(args[0], options)]);
      return print_module_binding(args[0], options);
    case "Pstr_recmodule":
    case "Pstr_modtype":
    case "Pstr_open":
    case "Pstr_class":
    case "Pstr_class_type":
    case "Pstr_include":
    case "Pstr_attribute":
    case "Pstr_extension":
      return [];
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function trim(str: string, ch: string[]) {
  let start = 0, end = str.length;

  while (start < end && ch.includes(str[start]))
    ++start;

  while (end > start && ch.includes(str[end - 1]))
    --end;

  return (start > 0 || end < str.length) ? str.substring(start, end) : str;
}

function is_empty(d: Doc): boolean {
  if (d instanceof Array)
    return d.every(is_empty);
  else if (d instanceof Object && d.hasOwnProperty("type")) {
    switch (d.type) {
      case "group":
        return is_empty(d.contents);
      case "fill":
        return is_empty(d.parts);
      default:
        return false;
    }
  }
  return false;
}

function trim_empty(ds: Doc): Doc[] {
  if (ds instanceof Array) {
    return ds.reduce<Doc[]>((acc, d) => {
      let d2 = trim_empty(d);
      if (is_empty(d2))
        return acc;
      else
        return acc.concat(d2);
    }, []);
  }
  else if (ds instanceof Object && ds.hasOwnProperty("type")) {
    switch (ds.type) {
      case "group":
        return [g(trim_empty(ds.contents))];
      case "fill":
        return [f(trim_empty(ds.parts))];
      default:
        return [ds];
    }
  }
  else if (is_empty(ds)) return []; else {
    return [ds];
  }
}

function print_structure_item(node: AST, options: Options): Doc {
  return g([print_structure_item_desc(node.pstr_desc, options)]);
}

function print_structure(node: AST, options: Options): Doc[] {
  return node.map(x => print_structure_item(x, options));
}

function print_toplevel_phrase(node: AST, options: Options): Doc {
  const constructor = node[0];
  const args = node.slice(1);
  switch (constructor) {
    case "Ptop_def": {
      try {
        const p = print_structure(args[0], options);
        // console.log(doc_to_string(p));
        return p;
      }
      catch (e) {
        console.log(e);
        // If something fails, just keep the original text.
        switch (args[0].length) {
          case 0: { throw e; }
          case 1: {
            const loc = args[0][0].pstr_loc;
            return get_source(loc, loc, options);
          }
          default: {
            const loc_start = args[0][0].pstr_loc;
            const loc_end = args[0][-1].pstr_loc;
            return get_source(loc_start, loc_end, options);
          }
        }
      }
    }
    case "Ptop_dir":
      return [];
    default:
      throw new Error(`Unexpected node type: ${constructor}`);
  }
}

function print(path: AstPath<Tree>, options: Options, _print: (path: AstPath<any>) => Doc): Doc {
  options.pattern_reals = [];
  options.move_everything_up = false;
  let r = trim_empty([path.node.top_defs.map(n => print_toplevel_phrase(n, options))]);
  return r;
}