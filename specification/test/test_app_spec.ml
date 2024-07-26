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
