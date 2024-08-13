[@@@import Math, "dune:math"]

[@@@import Specification, "dune:specification"]

open Math
open Specification

(* Let's use the spec-extracted library.*)

let some_check = Float8.nan = Float8.ninf;;

Printf.printf "NaN = -oo: %b\n" some_check

let x = Float8.negate Float8.pinf Format.B8P3;;

Printf.printf "+oo = %s\n" (Float8.to_string Float8.pinf Format.B8P3);;

Printf.printf "-oo = %s\n" (Float8.to_string Float8.ninf Format.B8P3);;

Printf.printf "(- +oo) = %s\n" (Float8.to_string x Format.B8P3);;

Printf.printf "NaN = NaN: %b\n" (Float8.nan = Float8.nan)

let chk_neg x fmt =
  Printf.printf "-(%s) = %s\n" (Float8.to_string x fmt)
    (Float8.to_string (Float8.negate x fmt) fmt)

let _ =
  chk_neg (Float8.of_int_bitwise Z.one) Format.B8P3;
  chk_neg (Float8.of_int_bitwise (Z.of_int 0x40)) Format.B8P3;
  chk_neg (Float8.of_int_bitwise (Z.of_int 0x7E)) Format.B8P3;
  chk_neg (Float8.of_int_bitwise (Z.of_int 0xFA)) Format.B8P3;
  chk_neg (Float8.of_int_bitwise (Z.of_int 0x78)) Format.B8P3;
  chk_neg (Float8.of_int_bitwise (Z.of_int 0xBD)) Format.B8P3;
  chk_neg (Float8.of_int_bitwise (Z.of_int 0x3B)) Format.B8P3;
  chk_neg (Float8.of_int_bitwise (Z.of_int 0xFD)) Format.B8P2

let chk_add x y fmt =
  Printf.printf "(%s + %s) = %s\n" (Float8.to_string x fmt)
    (Float8.to_string y fmt)
    (Float8.to_string
       (Float8.add x y fmt fmt fmt SaturationMode.OvfSat RoundingMode.TowardZero)
       fmt)

let _ =
  let one = Float8.of_int_bitwise (Z.of_int 0x40) in
  let two = Float8.of_int_bitwise (Z.of_int 0x44) in
  chk_add one one Format.B8P3;
  chk_add one two Format.B8P3;
  chk_add two one Format.B8P3;
  chk_add Float8.pinf one Format.B8P3

let _ =
  Printf.printf "(P1) 0xBB = %s\n"
    (ExReal.to_string
       (Float8.to_extended_real
          (Float8.of_int_bitwise (Z.of_int 0xBB))
          Format.B8P1))

let chk_add_scaled x y xscale yscale fmt =
  Printf.printf "(%s * 2^%s + %s * 2^%s) = %s\n" (Float8.to_string x fmt)
    (Z.to_string xscale) (Float8.to_string y fmt) (Z.to_string yscale)
    (Float8.to_string
       (Float8.add_scaled x y fmt fmt xscale yscale fmt SaturationMode.Other
          RoundingMode.TowardPositive)
       fmt)

let _ = chk_add_scaled Float8.pinf Float8.pinf Z.zero Z.zero Format.B8P1
