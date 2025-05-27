[@@@import Math, "dune:math"]

[@@@import Specification, "dune:specification"]

[@@@import AGM, "agm.iml"]

(* [@@@import Theorems_format_ranges, "theorems_format_ranges.iml"] *)

open Math
open Specification

(* Let's use the spec-extracted library.*)

let fse (kp : Specification.Format.kpt) : Specification.Format.t =
  { kp; s = Specification.Signedness.Signed; d = Specification.Domain.Extended }

let f = fse Specification.Format.B8P3

let some_check f = Ok (Float8.nan f) = Float8.ninf f;;

Printf.printf "NaN = -oo: %b\n" (some_check f);;

Printf.printf "+oo = %s\n" (Float8.to_string f (Result.get_ok (Float8.pinf f)));;

Printf.printf "-oo = %s\n" (Float8.to_string f (Result.get_ok (Float8.ninf f)))

let neg_pinf = Float8.negate f (Result.get_ok (Float8.pinf f));;

Printf.printf "(- +oo) = %s\n" (Float8.to_string f neg_pinf);;

Printf.printf "NaN = NaN: %b\n" (Float8.nan f = Float8.nan f)

let chk_neg x f =
  Printf.printf "-(%s) = %s\n" (Float8.to_string f x)
    (Float8.to_string f (Float8.negate f x))

let _ =
  let f = f in
  chk_neg (Float8.of_int_repr f Z.zero) f;
  chk_neg (Float8.of_int_repr f Z.one) f;
  chk_neg (Float8.of_int_repr f (Z.of_int 0x40)) f;
  chk_neg (Float8.of_int_repr f (Z.of_int 0x7E)) f;
  chk_neg (Float8.of_int_repr f (Z.of_int 0xFA)) f;
  chk_neg (Float8.of_int_repr f (Z.of_int 0x78)) f;
  chk_neg (Float8.of_int_repr f (Z.of_int 0xBD)) f;
  chk_neg (Float8.of_int_repr f (Z.of_int 0x3B)) f;
  chk_neg (Float8.of_int_repr f (Z.of_int 0xFD)) (fse Format.B8P2)

let chk_add f x y =
  Printf.printf "(%s + %s) = %s\n" (Float8.to_string f x) (Float8.to_string f y)
    (Float8.to_string f (Float8.add f f f (SaturationMode.SatPropagate, RoundingMode.TowardZero) x y))

let _ =
  let f = f in
  let one = Float8.of_int_repr f (Z.of_int 0x40) in
  let two = Float8.of_int_repr f (Z.of_int 0x44) in
  chk_add f one one;
  chk_add f one two;
  chk_add f two one;
  chk_add f (Result.get_ok (Float8.pinf f)) one

let result_to_string (x : ('a, string) Result.t) ~(f : 'a -> string) : string =
  match x with
  | Error e -> Printf.sprintf "Error: %s" e
  | Ok x -> Printf.sprintf "Ok %s" (f x)

let nan_or_exreal_to_string (x : Float8.NaNOrExReal.t) =
  match x with
  | Float8.NaNOrExReal.NaN -> "NaN"
  | Float8.NaNOrExReal.XR x -> ExReal.to_string x

let _ =
  Printf.printf "(P1) 0xBB = %s\n"
    (result_to_string ~f:nan_or_exreal_to_string
       (Float8.decode (fse Format.B8P1)
          (Float8.of_int_repr (fse Format.B8P1) (Z.of_int 0xBB))))

let chk_add_scaled x y s_x s_y f =
  Printf.printf "(%s * 2^%s + %s * 2^%s) = %s\n" (Float8.to_string f x)
    (Z.to_string s_x) (Float8.to_string f y) (Z.to_string s_y)
    (Float8.to_string f
       (Float8.add_scaled f f f
          (SaturationMode.SatPropagate, RoundingMode.TowardPositive)
          x s_x y s_y))

let _ =
  chk_add_scaled
    (Result.get_ok (Float8.pinf (fse Format.B8P1)))
    (Result.get_ok (Float8.pinf (fse Format.B8P1)))
    Z.zero Z.zero (fse Format.B8P1)

let print_decode x =
  let f = f in
  let fx = Float8.of_int_repr f (Z.of_int x) in
  let dx = Float8.decode f fx in
  Printf.printf "decode(%s) = %s\n" (Float8.to_string f fx)
    (result_to_string ~f:nan_or_exreal_to_string dx)

let _ =
  print_decode 0x00;
  print_decode 0x01;
  print_decode 0x80;
  print_decode 0xC0

let _ =
  let f = f in
  let x = Float8.NaNOrExReal.(XR (ExReal.of_int (Z.of_int (-1)))) in
  let ex = Float8.encode f x in
  Printf.printf "encode(%s) = %s\n"
    (nan_or_exreal_to_string x)
    (result_to_string ~f:(Float8.to_string f) ex)

let _ =
  let x = Q.of_int 0 in
  Printf.printf "x = %s\n" (Q.to_string x);
  let p = 3 in
  Printf.printf "p = %d\n" p;
  let b = 16 in
  Printf.printf "b = %d\n" b;
  let e = max (Util.floor_log2_abs x (Z.of_int 64)) (Z.of_int (1 - b)) in
  Printf.printf "E = %s\n" (Z.to_string e);
  let open ExReal.ResultInfix in
  let two = Z.of_int 2 in
  let s = rx x *. (two ^. Z.neg e) *. (two ^. Z.sub (Z.of_int p) Z.minus_one) in
  Printf.printf "S = %s\n" (result_to_string ~f:ExReal.to_string s);
  match s with
  | Ok (R s) ->
    let s = Q.to_int s in
    let t = s mod Z.to_int (Util.ipow2 (Z.of_int (p - 1))) in
    Printf.printf "T = %d\n" t
  | Error e -> Printf.printf "error: %s" e
  | _ -> Printf.printf "Non-R"

let _ =
  let x =
    Float8.round_to_precision (Z.of_int 3) (Z.of_int 16)
      RoundingMode.TowardPositive
      (ExReal.R (Q.of_ints 21 10))
  in
  Printf.printf "Rounded: %s\n%!" (ExReal.to_string x)
