[@@@import Math, "dune:math"]

[@@@import Specification, "dune:specification"]

[@@@import AGM, "agm.iml"]

[@@@import Sqrt, "sqrt.iml"]

[@@@import Theorems_sqrt, "dune:theorems"]

open Math
open Specification
open Theorems

(*
let _ =
  let open Format in
  let formats =
    [
      B3P1;
      B3P2;
      B6P1;
      B6P2;
      B6P3;
      B6P4;
      B6P5;
      B7P1;
      B7P2;
      B7P3;
      B7P4;
      B7P5;
      B7P6;
      B8P1;
      B8P2;
      B8P3;
      B8P4;
      B8P5;
      B8P6;
      B8P7;
      B14P1;
      B14P2;
      B14P3;
      B14P4;
      B14P5;
      B14P6;
      B14P7;
      B14P8;
      B14P9;
      B14P10;
      B14P11;
      B14P12;
      B14P13;
      B15P1;
      B15P2;
      B15P3;
      B15P4;
      B15P5;
      B15P6;
      B15P7;
      B15P8;
      B15P9;
      B15P10;
      B15P11;
      B15P12;
      B15P13;
      B15P14;
    ]
  in
  List.iter
    (fun f ->
      let k, p, _, _ = Format.get_format_parameters f in
      Printf.printf "B%sP%s\n%!" (Z.to_string k) (Z.to_string p);
      for i = 0 to 2 lsl Z.to_int k do
        let q =
          Theorems.Theorems_format_ranges.is_within_range f (Z.of_int i)
        in
        if not q then Printf.printf "%b\n" q
      done)
    formats
*)

let rat_to_string_dec (x : Q.t) : string =
  let open Bignum in
  of_bigint (Bigint.of_zarith_bigint (Q.num x))
  / of_bigint (Bigint.of_zarith_bigint (Q.den x))
  |> to_string_hum ~decimals:128

(* let () =
  let f =
    { Format.kp = Format.B8P7; s = Signedness.Signed; d = Domain.Extended }
  in
  let i = 0x51 in
  let prev_over, prev_under = ref Q.zero, ref Q.zero in
  for p = 1 to 32 do
    let start = Unix.gettimeofday () in
    let x = Float.of_int_repr f (Z.of_int i) in
    match Float.decode f x with
    | NaN | PINF | NINF -> ()
    | R xr ->
      let rto = Sqrt.sqrt_over xr (Z.of_int p) in
      let rtu = Sqrt.sqrt_under xr (Z.of_int p) in
      (match rtu, rto with
      | Ok rtu, Ok rto ->
      (* | Ok rto -> *)
        (* let rtu = if Q.equal rto Q.zero then Q.zero else Q.div xr rto in *)
        Printf.printf "\b%03d=%s: p=%d: [%s%s%s, %s%s%s]%s%!" i
          (Float.to_string f x) p
          (if Q.equal rtu !prev_under then "=" else "")
          (if Q.equal (Q.mul rtu rtu) xr then "X" else "")
          (rat_to_string_dec rtu)
          (if Q.equal rto !prev_over then "=" else "")
          (if Q.equal (Q.mul rto rto) xr then "X" else "")
          (rat_to_string_dec rto)
          (if Q.lt rtu rto then ""
           else if Q.equal rtu rto then " =!="
           else " ERR");
        prev_over := rto;
        prev_under := rtu
      | _ -> Printf.printf "\b%03d=%s: p=%d: error%!" i (Float.to_string f x) p);
      let stop = Unix.gettimeofday () in
      Printf.printf " %0.2fs\n%!" (stop -. start)
  done

let () =
  let f =
    { Format.kp = Format.B8P7; s = Signedness.Signed; d = Domain.Extended }
  in
  let pi = SaturationMode.OvfInf, RoundingMode.TowardPositive in
  let i = 81 in
  for p = 0 to 32 do
    let x = Float.of_int_repr f (Z.of_int i) in
    let rto = Theorems_sqrt.fsqrt_over f f pi x (Z.of_int p) in
    let rtu = Theorems_sqrt.fsqrt_under f f pi x (Z.of_int p) in
    Printf.printf "\b%03d=%s: p=%d: [%s, %s] (d %s) \n%!" i
      (Float.to_string f x) p (Float.to_string f rtu) (Float.to_string f rto)
      (Z.to_string (Z.sub rto rtu))
  done *)

(* let () =
  let f =
    { Format.kp = Format.B8P1; s = Signedness.Signed; d = Domain.Extended }
  in
  let pi = SaturationMode.SatPropagate, RoundingMode.NearestTiesToEven in
  let num_disagree = ref 0 in
  let p = 18 in
  for i = 0 to (2 lsl (Z.to_int (Format.k f) - 1)) - 1 do
    (* for i = 97 to 2 lsl Z.to_int (Format.k f) do *)
    let start = Unix.gettimeofday () in
    let x = Float.of_int_repr f (Z.of_int i) in
    let rto =
      Theorems_sqrt.fsqrt_over f f
        (fst pi, RoundingMode.TowardNegative)
        x (Z.of_int p)
    in
    let rtu =
      Theorems_sqrt.fsqrt_under f f
        (fst pi, RoundingMode.TowardPositive)
        x (Z.of_int p)
    in
    if rto != rtu && rto > rtu then (
      Printf.printf "%02x=%03d=%s: [%s, %s] (d %s)%!" i i (Float.to_string f x)
        (Float.to_string f rtu) (Float.to_string f rto)
        (Z.to_string (Z.sub rto rtu));
      num_disagree := !num_disagree + 1 (* Printf.printf ".%!" *))
    else
      Printf.printf "%02x=%03d=%s: [%s, %s]%!" i i (Float.to_string f x)
        (Float.to_string f rtu) (Float.to_string f rto);
    let stop = Unix.gettimeofday () in
    Printf.printf " %0.2fs\n%!" (stop -. start)
  done;
  Printf.printf "\b# disagreements found: %d\n%!" !num_disagree *)

let sprefix (s : string) (i : int) = String.sub s 0 (min i (String.length s))

let augreal_dec (x : AugReal.t) : string =
  match x with
  | NaN | PINF | NINF -> AugReal.to_string x
  | R r -> rat_to_string_dec r

let r_of_aug (x : AugReal.t) =
  let open AugReal in
  match x with
  | R r -> Ok r
  | _ -> Error "unreachable"

(* let rec fsqrt_aux prec bias f rnd (x : Q.t) (p : Z.t) : (Q.t, string) Result.t =
  match Sqrt.sqrt_over x p, Sqrt.sqrt_under x p with
  | Ok o, Ok u ->
    Printf.printf "p=%s: [%s,%s] " (Z.to_string p)
      (sprefix (rat_to_string_dec u) 32)
      (sprefix (rat_to_string_dec o) 32);
    let ornded = Float.round_to_precision prec bias rnd (AugReal.of_real o) in
    let urnded = Float.round_to_precision prec bias rnd (AugReal.of_real u) in
    Printf.printf "[%s,%s]\n%!" (augreal_dec urnded) (augreal_dec ornded);
    if ornded = urnded then
      (* Printf.printf "Match\n%!"; *)
      r_of_aug ornded
    else if AugReal.( * ) ornded ornded = Ok (AugReal.R x) then (
      Printf.printf "Exact RO\n%!";
      r_of_aug ornded)
    else if AugReal.( * ) urnded urnded = Ok (AugReal.R x) then (
      Printf.printf "Exact RU\n%!";
      r_of_aug urnded)
    else if Q.mul o o = x then (
      Printf.printf "Exact O\n%!";
      Ok o)
    else if Q.mul u u = x then (
      Printf.printf "Exact U\n%!";
      Ok u)
    else fsqrt_aux prec bias f rnd x (Z.add p Z.one)
  | Error e, _ -> Error e
  | _, Error e -> Error e *)

let fsqrt_aux_both prec bias f rnd
    (start : Q.t -> Z.t -> Sqrt.state * Sqrt.state) (x : Q.t) (p : Z.t) :
    (Q.t * Float.t, string) Result.t =
  let early_exit (u : Sqrt.state) (o : Sqrt.state) =
    Printf.printf "i=%s: [%s,%s] " (Z.to_string o.i)
      (sprefix (rat_to_string_dec u.g) 24)
      (sprefix (rat_to_string_dec o.g) 24);
    let urnded = Float.round_to_precision prec bias rnd (AugReal.R u.g) in
    let ornded = Float.round_to_precision prec bias rnd (AugReal.R o.g) in
    Printf.printf "[%s,%s] [%02x,%02x]\n%!"
      (sprefix (augreal_dec urnded) 24)
      (sprefix (augreal_dec ornded) 24)
      (match Float.encode f urnded with
      | Ok urf -> Z.to_int (Float.to_int_repr f urf)
      | _ -> 0xFF)
      (match Float.encode f ornded with
      | Ok orf -> Z.to_int (Float.to_int_repr f orf)
      | _ -> 0xFF);
    if ornded = urnded then
      (* Printf.printf "Match\n%!"; *)
      (* r_of_aug ornded *)
      true
    else if AugReal.( * ) ornded ornded = Ok (AugReal.R x) then (
      Printf.printf "Exact RO\n%!";
      (* r_of_aug ornded) *)
      true
      (* else if AugReal.( * ) urnded urnded = Ok (AugReal.R x) then (
      Printf.printf "Exact RU\n%!";
      (* r_of_aug urnded *)
      true) *))
    else if Q.mul o.g o.g = x then (
      Printf.printf "Exact O\n%!";
      (* Ok o *)
      true)
    (* else if Q.mul u.g u.g = x then (
      Printf.printf "Exact U\n%!";
      (* Ok u *)
      true) *)
      else false
  in
  Sqrt.sqrt_both start early_exit x p

let rec find_closest_floats_aux_ml (f : Format.t) (x : AugReal.t) (l : Float.t)
    (u : Float.t) : (Float.t * Float.t, string) Result.t =
  (* Printf.printf "[%02x,%02x]\n%!" (Z.to_int l) (Z.to_int u); *)
  if Z.lt l Z.zero || Z.lt u Z.zero then Error "invalid negative"
  else if l > u then Error "invalid interval"
  else if Z.sub u l <= Z.one then Ok (l, u)
  else (
    let m = Z.div (Z.add l u) (Z.of_int 2) in
    let dm = Float.decode f m in
    match AugReal.(dm * dm) with
    | Ok dmsq when AugReal.(dmsq >= x) -> find_closest_floats_aux_ml f x l m
    | Ok _ -> find_closest_floats_aux_ml f x m u
    | Error e -> Error e)

let find_closest_floats_ml (f : Format.t) (x : AugReal.t) =
  let k, p, bias, m_lo, m_hi, _, _ = Format.get_format_parameters f in
  find_closest_floats_aux_ml f x Z.zero
    (if f.s = Signedness.Signed then
       if f.d = Domain.Extended then Z.sub (Util.ipow2 (Z.sub k Z.one)) Z.one
       else Util.ipow2 (Z.sub k Z.one)
     else if f.d = Domain.Extended then Z.sub (Util.ipow2 k) Z.one
     else Util.ipow2 k)

(* let () =
  let f =
    {
      Format.kp = Format.B8P3;
      s = Specification.Signedness.Signed;
      d = Specification.Domain.Extended;
    }
  in
  let k, p, bias, _, _, _, _ = Format.get_format_parameters f in
  for i = 0 to (2 lsl (Z.to_int k - 2)) - 2 do
    Printf.printf "%02x: " i;
    let di = Float.decode f (Float.of_int_repr f (Z.of_int i)) in
    match find_closest_floats_ml f di with
    | Ok (l, u) ->
      Printf.printf "sqrt(%s) in [%02x,%02x]\n%!" (augreal_dec di) (Z.to_int l)
        (Z.to_int u)
    | Error e -> Printf.printf "Error: %s\n%!" e
  done *)

let signedness_of_i i =
  let open Signedness in
  match i with
  | 0 -> Signed
  | _ -> Unsigned

let domain_of_i i =
  let open Domain in
  match i with
  | 0 -> Extended
  | _ -> Finite

let saturation_mode_of_i i =
  let open SaturationMode in
  match i with
  | 0 -> SatFinite
  | 1 -> SatPropagate
  | _ -> OvfInf

let rounding_mode_of_i i =
  let open RoundingMode in
  match i with
  | 0 -> TowardZero
  | 1 -> TowardNegative
  | 2 -> TowardPositive
  | 3 -> NearestTiesToEven
  | _ -> NearestTiesToAway

let forall_formats (fn : Format.t -> unit) : unit =
  for k = 2 to 15 do
    for p = 1 to k do
      for sj = 0 to 1 do
        for dj = 0 to 1 do
          match
            Format.of_parts (Z.of_int k) (Z.of_int p) (signedness_of_i sj)
              (domain_of_i dj)
          with
          | Error e -> Printf.printf "Invalid format: %d/%d/%d/%d\n%!" k p sj dj
          | Ok f -> fn f
        done
      done
    done
  done

let forall_projections (fn : Projection.t -> unit) : unit =
  for i = 0 to 2 do
    for j = 0 to 4 do
      fn (saturation_mode_of_i i, rounding_mode_of_i j)
    done
  done

let print_closest (f_z : Format.t) (di : AugReal.t) =
  let open AugReal in
  match di with
  | NaN | NINF | PINF -> ()
  | R s when Q.lt s Q.zero -> ()
  | R s ->
    (match Fsqrt.find_closest_sqrt_floats f_z di with
    | Ok (u, o) ->
      Printf.printf "Closest: [%s,%s]\n%!" (Z.to_string u) (Z.to_string o);
      (match r_of_aug (Float.decode f_z u), r_of_aug (Float.decode f_z o) with
      | Ok u, Ok o ->
        Printf.printf "[%s,%s]\n%!" (Q.to_string u) (Q.to_string o)
      | _ -> Printf.printf "Error\n%!")
    | Error e -> Printf.printf "Closest error: %s\n%!" e)

let run_sqrt () =
  let sep_line = String.make 80 '-' in
  let star_line = String.make 80 '*' in
  let open Signedness in
  let open Domain in
  forall_formats (fun f_x ->
      forall_formats (fun f_z ->
          forall_projections (fun pi ->
              Printf.printf "%s\n" star_line;
              Printf.printf "%s -> %s, %s\n%!" (Format.to_string f_x)
                (Format.to_string f_z) (Projection.to_string pi);
              let k, _, _, _, _, s, d = Format.get_format_parameters f_x in
              let max =
                match s, d with
                | Signed, Extended -> (2 lsl (Z.to_int k - 1)) - 2
                | Signed, Finite -> 2 lsl (Z.to_int k - 2)
                | Unsigned, Extended -> (2 lsl (Z.to_int k - 1)) - 1
                | Unsigned, Finite -> (2 lsl (Z.to_int k - 1)) - 1
              in
              for i = 0 to max do
                let start = Unix.gettimeofday () in
                let fi = Float.of_int_repr f_x (Z.of_int i) in
                let di = Float.decode f_x fi in
                if AugReal.(di >= zero) then (
                  if false then (
                    Printf.printf "%s\n" sep_line;
                    Printf.printf "0x%02x = %s = %s\n%!" i
                      (AugReal.to_string di) (augreal_dec di);
                    print_closest f_z di)
                  else Printf.printf "0x%02x = %s; " i (AugReal.to_string di);
                  (match Fsqrt.fsqrt_i f_x f_z pi fi with
                  | Ok fo, n ->
                    Printf.printf "sqrt(%s) = %s (0x%02x), %s it, "
                      (augreal_dec di) (Float.to_string f_z fo)
                      (Z.to_int (Float.to_int_repr f_z fo))
                      (Z.to_string n);
                    if Z.gt n (Z.of_int 2) then (
                      Printf.printf " CHECK ME! ";
                      Printf.eprintf "CHECK!\n%!")
                  | Error e, _ -> Printf.printf "Error: %s" e);
                  Printf.printf "%0.2fs\n%!" (Unix.gettimeofday () -. start))
              done)))

let () =
  for isat = 0 to 2 do
    for irnd = 0 to 4 do
      let start = Unix.gettimeofday () in
      let pi = saturation_mode_of_i isat, rounding_mode_of_i irnd in
      Printf.printf "(%d,%d) ... %!" isat irnd;
      let r =
        Theorems_sqrt_convergence.forall_pairs_of_formats
          Theorems_sqrt_convergence.fsqrt_converges pi
      in
      Printf.printf "%b %0.2fs\n%!" r (Unix.gettimeofday () -. start)
    done
  done
