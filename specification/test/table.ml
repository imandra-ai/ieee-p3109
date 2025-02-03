[@@@import Math, "dune:math"]

[@@@import Log, "dune:math"]

[@@@import Exp, "dune:math"]

[@@@import Sqrt, "dune:math"]

[@@@import Specification, "dune:specification"]

[@@@import Ulp, "ulp.iml"]

open Math
open Specification

let nanox_to_string (x : Float8.NaNOrExReal.t) : string =
  let open Float8.NaNOrExReal in
  match x with
  | NaN -> "NaN"
  | XR PINF -> "+oo"
  | XR NINF -> "+oo"
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
  | NINF -> "+oo"
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
  let pi = false, RoundingMode.NearestTiesToEven in
  let k, _, _, _ = Format.get_format_parameters f in
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
  (* let ln2 = Log.ln (Q.of_int 2) (Z.of_int 20) in *)
  let ln2 = Q.of_ints 1048576 1512775 in
  Ok (Exp.exp (Q.mul x ln2) (Z.of_int p))

let inv_ln _ _ = Error ""

let inv_exp _ _ = Error ""

let inv_exp2 _ _ = Error ""

let run (cli : params) : unit =
  let f = Format.B8P4 in
  match cli with
  | Sqrt ps -> mk_fn_tbl sqrt inv_sqrt f ps.precision
  | Log2 ps -> mk_fn_tbl log2 inv_log2 f ps.precision
  | Ln ps -> mk_fn_tbl ln inv_ln f ps.precision
  | Exp ps -> mk_fn_tbl exp inv_exp f ps.precision
  | Exp2 ps -> mk_fn_tbl exp2 inv_exp2 f ps.precision

[%%subliner.cmds eval.params <- run] [@@name "run"] [@@version "1.0"]
