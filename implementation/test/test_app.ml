open Implementation

(* Let's use our P3109 library. *)

let some_check = Float.nan = Float.ninf;;

Printf.printf "NaN = -oo: %b\n" some_check

let fse (kp : Specification.Format.kpt) : Specification.Format.t =
  { kp; s = Specification.Signedness.Signed; d = Specification.Domain.Extended }

let f = fse Specification.Format.B8P3

let x = Float.negate f (Float.pinf f);;

Printf.printf "+oo = %s\n" (Float.to_string f (Float.pinf f));;

Printf.printf "-oo = %s\n" (Float.to_string f (Float.ninf f));;

Printf.printf "(- +oo) = %s\n" (Float.to_string f x);;

Printf.printf "NaN = NaN: %b\n" (Float.nan = Float.nan)

let fmt = Specification.Format.B8P3
