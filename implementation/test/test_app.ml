open Implementation

(* Let's use our P3109 library. *)

let some_check = Float8.nan = Float8.ninf;;

Printf.printf "NaN = -oo: %b\n" some_check

let x = Float8.negate Float8.pinf B8P3;;

Printf.printf "+oo = %s\n"
  (Float8.to_string Float8.pinf Specification.Format.B8P3)
;;

Printf.printf "-oo = %s\n"
  (Float8.to_string Float8.ninf Specification.Format.B8P3)
;;

Printf.printf "(- +oo) = %s\n" (Float8.to_string x Specification.Format.B8P3);;

Printf.printf "NaN = NaN: %b\n" (Float8.nan = Float8.nan)

let fmt = Specification.Format.B8P3

let x = Float8.of_int_bitwise Z.one;;

Printf.printf "-(%s) = %s\n" (Float8.to_string x fmt)
  (Float8.to_string (Float8.negate x fmt) fmt)
