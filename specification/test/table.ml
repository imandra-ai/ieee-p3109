[@@@import Math, "dune:math"]

[@@@import Log, "dune:math"]

[@@@import Exp, "dune:math"]

[@@@import Sqrt, "dune:math"]

[@@@import Specification, "dune:specification"]
[@@@import Theorems, "dune:theorems"]


open Math
open Specification
open Theorems

let signedness_to_string (s : Signedness.t) =
  match s with
  | Signed -> "signed"
  | Unsigned -> "unsigned"

let domain_to_string (d : Domain.t) =
  match d with
  | Finite -> "finite"
  | Extended -> "extended"

let signedness_to_char (s : Signedness.t) =
  match s with
  | Signed -> "s"
  | Unsigned -> "u"

let domain_to_char (d : Domain.t) =
  match d with
  | Finite -> "f"
  | Extended -> "e"

let nanox_to_string (x : Float8.NaNOrExReal.t) : string =
  let open Float8.NaNOrExReal in
  match x with
  | NaN -> "NaN"
  | XR PINF -> "+oo"
  | XR NINF -> "-oo"
  | XR (R r) -> Q.to_string r

let rat_to_string_dec (x : Q.t) : string =
  let open Bignum in
  Printf.sprintf "%s"
    (of_bigint (Bigint.of_zarith_bigint (Q.num x))
     / of_bigint (Bigint.of_zarith_bigint (Q.den x))
    |> to_string_hum ~decimals:32)

let exreal_to_string_dec (x : ExReal.t) : string =
  match x with
  | PINF -> "+oo"
  | NINF -> "-oo"
  | R r -> Printf.sprintf "%s" (rat_to_string_dec r)

let nanox_to_dec_string (x : Float8.NaNOrExReal.t) : string =
  let open Float8.NaNOrExReal in
  match x with
  | NaN -> "NaN"
  | XR xr -> exreal_to_string_dec xr

let print_nanox_dec (x : Float8.NaNOrExReal.t) =
  Printf.printf "%s," (nanox_to_dec_string x)

let print_real (x : Q.t) = Printf.printf "%s," (rat_to_string_dec x)

let print_exreal_dec (x : ExReal.t) =
  Printf.printf "%s," (exreal_to_string_dec x)

let print_bool (x : bool) = Printf.printf "%s," (if x then "Y" else "N")

let mk_fn_tbl (fn : Q.t -> int -> (bool * Q.t, string) Result.t)
    (inv_fn : Q.t -> int -> (Q.t, string) Result.t) (f : Format.t) (p : int) :
    unit =
  let open Float8.NaNOrExReal in
  let pi = SaturationMode.SatPropagate, RoundingMode.NearestTiesToEven in
  let k, _, _, _, _, _, _ = Format.get_format_parameters f in
  Printf.printf
    "\"in int\",\"in dec\",\"out \
     real*\",\"precise?\",\"rev*\",\"diff*\",\"ulp\",\"within?\",\"out \
     int\",\"out bin\",\"out dec\",\"\"\n\
     %!";

  for i = 0 to (2 lsl (Z.to_int k - 1)) - 1 do
    Printf.printf "0x%02x," i;

    let fp = Float8.of_int_repr f (Z.of_int i) in
    (match Float8.decode f fp with
    | Ok decoded ->
      print_nanox_dec decoded;
      (match decoded with
      | XR dec_xr ->
        (match dec_xr with
        | R d ->
          (match fn d p with
          | Ok (precise, out_real) ->
            print_real out_real;
            print_bool precise;
            (match inv_fn out_real (p * p) with
            | Ok rev ->
              print_real rev;
              let diff = Q.sub rev d in
              print_real diff;
              (match Ulp.ulp f fp with
              | Ok ulp ->
                print_real ulp;
                print_bool (Q.lt (Q.abs diff) ulp)
              | Error _ -> Printf.printf "?,?,")
            | Error e -> Printf.printf "%s," e);
            (match Float8.project f pi (R out_real) with
            | Ok projected ->
              Printf.printf "0x%s,%s,"
                (Z.format "x" (Float8.to_int_repr f projected))
                (Z.format "08b" (Float8.to_int_repr f projected));
              (match Float8.decode f projected with
              | Ok out_dec -> print_nanox_dec out_dec
              | Error e -> Printf.printf "%s," e)
            | Error e -> Printf.printf "%s," e)
          | Error e -> Printf.printf "%s" e)
        | _ -> Printf.printf "%s" (nanox_to_string decoded))
      | _ -> Printf.printf "%s" (nanox_to_string decoded))
    | Error e -> Printf.printf "decoding error: %s" e);
    Printf.printf "\n%!"
  done

type subparams = { precision: int [@default 8] } [@@deriving subliner]

type params =
  | Sqrt of subparams
  | Log2 of subparams
  | Ln of subparams
  | Exp of subparams
  | Exp2 of subparams
  | Format of {
      k: int; [@pos 0]
      p: int; [@pos 1]
      s: bool; [@pos 2]
      e: bool; [@pos 3]
    }
  | Formats
[@@deriving subliner]

let sqrt x p =
  match Sqrt.sqrt x (Z.of_int p) with
  | Ok x -> Ok (false, x)
  | Error e -> Error e

let inv_sqrt x _ = Ok (Q.mul x x)

let log2 x p = Log.log2_plus x (Z.of_int p)

let ln x p =
  match Log.ln x (Z.of_int p) with
  | Ok x -> Ok (false, x)
  | Error e -> Error e

let exp x p = Ok (false, Exp.exp x (Z.of_int p))

let exp2 x p =
  match Exp.exp2 x (Z.of_int p) with
  | Ok x -> Ok (false, x)
  | Error e -> Error e

let ln2 p : Q.t =
  match Log.ln (Q.of_int 2) (Z.of_int p) with
  | Ok r -> r
  | Error _ -> assert false

let inv_log2 (x : Q.t) (p : int) : (Q.t, string) Result.t =
  (* let ln2 = Q.of_ints 1048576 1512775 in *)
  let ln2 = Q.of_ints 4194304 6051101 in
  Ok (Exp.exp (Q.mul x ln2) (Z.of_int p))

let inv_ln _ _ = Error ""

let inv_exp _ _ = Error ""

let inv_exp2 _ _ = Error ""

let int2bin_str i n : string =
  let i = ref i in
  let r = ref "" in
  for _ = 0 to n - 1 do
    if !i mod 2 = 1 then r := "1" ^ !r else r := "0" ^ !r;
    i := !i / 2
  done;
  !r

exception InvalidFormat of string

let mk_f_tbl (k : int) (p : int) (s : bool) (e : bool) =
  let f : Format.t =
    match Format.of_kp (Z.of_int k) (Z.of_int p) with
    | Ok kp ->
      {
        kp;
        s = (if s then Signed else Unsigned);
        d = (if e then Extended else Finite);
      }
    | Error e -> raise (InvalidFormat e)
  in
  let kf, pf, bias, _, m_hi, s, d = Format.get_format_parameters f in
  Printf.printf "B%sP%s%s%s: bias = %s, m_hi = %s\n\n" (Z.to_string kf)
    (Z.to_string pf) (domain_to_char d) (signedness_to_char s)
    (Z.to_string bias) (rat_to_string_dec m_hi);
  for i = 0 to (1 lsl k) - 1 do
    let iz = Float8.to_int_repr f (Z.of_int i) in
    let d = Float8.decode f iz in
    let ds =
      match d with
      | Ok r -> nanox_to_dec_string r
      | Error e -> Printf.sprintf "Error: %s" e
    in
    Printf.printf "%02x | %s | %s\n" i (int2bin_str i k) ds
  done

let run (cli : params) : unit =
  try
    (* let ln2 = Log.ln (Q.of_int 2) (Z.of_int 21) in
  (match ln2 with
  | Ok ln2 -> Printf.printf "ln(2) = %s%!" (Q.to_string ln2)
  | Error _ -> ()); *)
    let f : Format.t =
      { kp = Format.B8P4; s = Signedness.Signed; d = Domain.Extended }
    in
    match cli with
    | Sqrt ps -> mk_fn_tbl sqrt inv_sqrt f ps.precision
    | Log2 ps -> mk_fn_tbl log2 inv_log2 f ps.precision
    | Ln ps -> mk_fn_tbl ln inv_ln f ps.precision
    | Exp ps -> mk_fn_tbl exp inv_exp f ps.precision
    | Exp2 ps -> mk_fn_tbl exp2 inv_exp2 f ps.precision
    | Format { k; p; s; e } -> mk_f_tbl k p s e
    | Formats ->
      for k = 3 to 8 do
        for p = 1 to k - 1 do
          for s = 0 to 1 do
            for d = 0 to 1 do
              Printf.printf "-------------------------\n";
              mk_f_tbl k p (s == 0) (d == 0);
              Printf.printf "\n\n"
            done
          done
        done
      done
  with exc -> Printf.printf "Exception: %s\n" (Printexc.to_string exc)

[%%subliner.cmds eval.params <- run] [@@name "run"] [@@version "1.0"]
