open Specification

(* Let's use the spec-extracted library.*)

let some_check = Float8.nan = Float8.ninf;;

Printf.printf "NaN = -oo: %b\n" some_check

let x = Float8.negate Float8.pinf Format.B8P3;;

Printf.printf "+oo = %s\n" (Float8.to_string Float8.pinf Format.B8P3);;

Printf.printf "-oo = %s\n" (Float8.to_string Float8.ninf Format.B8P3);;

Printf.printf "(- +oo) = %s\n" (Float8.to_string x Format.B8P3);;

Printf.printf "NaN = NaN: %b\n" (Float8.nan = Float8.nan)

let fmt = Format.B8P3

let tiny = Float8.of_int_bitwise Z.one;;

Printf.printf "-(%s) = %s\n"
  (Float8.to_string tiny fmt)
  (Float8.to_string (Float8.negate tiny fmt) fmt)

let one = Float8.of_int_bitwise (Z.of_int 0x40);;

Printf.printf "-(%s) = %s\n" (Float8.to_string one fmt)
  (Float8.to_string (Float8.negate one fmt) fmt)
