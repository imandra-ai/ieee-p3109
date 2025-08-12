import os
import sys
import pandas as pd
import re
from enum import Enum
from fractions import Fraction

julia_paths = ["signed", "unsigned"]
spec_path = "spec-formats"


class Signedness(Enum):
    Signed = 1
    Unsigned = 2


class Domain(Enum):
    Extended = 1
    Finite = 2


format_spec_re = re.compile("binary([0-9]+)p([0-9]+)(s|u)(e|f)")
hexfloat_re = re.compile(r"([+-]?)0x([0-9a-fA-F]+)(?:\.([0-9a-fA-F]+))?p([+-]?\d+)")


def format_specs(name):
    ms = format_spec_re.match(name)
    if ms:
        k = int(ms.group(1))
        p = int(ms.group(2))
        s = Signedness.Signed if ms.group(3) == "s" else Signedness.Unsigned
        d = Domain.Extended if ms.group(4) == "e" else Domain.Finite
        return k, p, s, d
    else:
        print("no match")
        return None


def is_special(hf):
    return hf == "NaN" or hf == "Inf" or hf == "-Inf"


def parse_hexfloat(hf):
    if is_special(hf):
        return hf
    else:
        ms = hexfloat_re.match(hf)
        if ms:
            sgn, sig_int, sig_frac, exp = ms.groups()
            sgn = 1.0 if sgn == "+" or sgn == "" or sgn is None else -1.0
            isig = int(sig_int, 16) if sig_int else 0
            fsig = 0.0
            fsigf = Fraction(0)
            if sig_frac:
                for i, digit in enumerate(sig_frac):
                    digit_value = int(digit, 16)
                    fsigf += Fraction(digit_value) / (16 ** (i + 1))
                    fsig += digit_value / (16 ** (i + 1))
            sig = Fraction(isig) + fsigf
            return sgn, sig, int(exp)
        else:
            return None


def compare(format_name, julia_df, spec_df):
    # k, p, s, d = format_specs(format_name)
    # print(f"k={k} p={p} s={s.name} d={d.name}")
    # print(df)
    num_errors = 0
    num_ok = 0
    for i, code, hf in julia_df.itertuples():
        hf = hf.strip(" ")
        code = int(code, base=16)
        spec_num = spec_df[2][i]
        if is_special(hf):
            sspec_num = str(spec_num)
            if not (
                (hf == "NaN" and sspec_num == "nan")
                or (hf == "Inf" and sspec_num == "+oo")
                or (hf == "-Inf" and sspec_num == "-oo")
            ):
                print(f"{code:02X}: {hf} != {spec_num}")
                sys.stdout.flush()
                num_errors = num_errors + 1
            else:
                num_ok = num_ok + 1
        else:
            sgn, sig, exp = parse_hexfloat(hf)
            julia_num = (
                Fraction(sgn) * Fraction(sig) * Fraction(Fraction(2) ** Fraction(exp))
            )
            if Fraction(julia_num) != Fraction(spec_num):
                print(f"{code:02X}: {hf} = {julia_num} != {spec_num}")
                sys.stdout.flush()
                num_errors = num_errors + 1
            else:
                num_ok = num_ok + 1
    if num_errors > 0:
        print(f"{format_name}: {num_errors} ✕")
    else:
        print(f"{format_name}: {num_ok} ✓")


def main(argv):
    sys.set_int_max_str_digits(2**16)

    for p in julia_paths:
        for root, dirs, files in os.walk(p):
            for file in files:
                if file.endswith(".csv"):
                    path = os.path.join(root, file)
                    julia_df = pd.read_csv(path, skipinitialspace=True)
                    julia_df.columns = julia_df.columns.str.strip()
                    for col in julia_df.columns:
                        if col == "code":
                            continue
                        format_name = col
                        k, p, s, d = format_specs(format_name)
                        if k >= 2 and k <= 15:
                            spec_df = pd.read_csv(
                                os.path.join(spec_path, format_name + ".csv"),
                                skipinitialspace=True,
                                header=None,
                            )
                            compare(format_name, julia_df[["code", col]], spec_df)
                            sys.stdout.flush()
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
