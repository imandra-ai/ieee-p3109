open P3109

(* Let's use our P3109 library.*)

let some_check = Float8.nan = Float8.ninf;;

Printf.printf "NaN = -oo: %b\n" some_check

let x = Float8.negate Float8.pinf Format.B8P3;;

Printf.printf "+oo = %s\n" (Float8.to_string Float8.pinf);;

Printf.printf "-oo = %s\n" (Float8.to_string Float8.ninf);;

Printf.printf "(- +oo) = %s\n" (Float8.to_string x);;

Printf.printf "NaN = NaN: %b\n" (Float8.NaN = Float8.NaN)
