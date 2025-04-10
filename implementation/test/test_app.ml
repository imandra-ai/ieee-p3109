open Implementation

(* Let's use our P3109 library. *)

let some_check = Float8.nan = Float8.ninf;;

Printf.printf "NaN = -oo: %b\n" some_check

let fse (kp : Specification.Format.kpt) : Specification.Format.t =
  { kp; s = Specification.Signedness.Signed; d = Specification.Domain.Extended }

let f = fse Specification.Format.B8P3

let x = Float8.negate f (Float8.pinf f);;

Printf.printf "+oo = %s\n" (Float8.to_string f (Float8.pinf f));;

Printf.printf "-oo = %s\n" (Float8.to_string f (Float8.ninf f));;

Printf.printf "(- +oo) = %s\n" (Float8.to_string f x);;

Printf.printf "NaN = NaN: %b\n" (Float8.nan = Float8.nan)

let fmt = Specification.Format.B8P3
