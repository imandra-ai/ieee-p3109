open Implementation

(* Let's use our P3109 library. *)

let some_check = Float8.nan = Float8.ninf;;

Printf.printf "NaN = -oo: %b\n" some_check

let x = Float8.negate B8P3 Float8.pinf;;

Printf.printf "+oo = %s\n"
  (Float8.to_string Specification.Format.B8P3 Float8.pinf)
;;

Printf.printf "-oo = %s\n"
  (Float8.to_string Specification.Format.B8P3 Float8.ninf)
;;

Printf.printf "(- +oo) = %s\n" (Float8.to_string Specification.Format.B8P3 x);;

Printf.printf "NaN = NaN: %b\n" (Float8.nan = Float8.nan)

let fmt = Specification.Format.B8P3

let x = Float8.of_int_bitwise Z.one;;

Printf.printf "-(%s) = %s\n" (Float8.to_string fmt x)
  (Float8.to_string fmt (Float8.negate fmt x))
