#!/usr/bin/env python3
"""Generate the Bezout certificate used in trig_zeros.iml.

The Taylor sections of sin and cos at section index p, written over u = x^2:

  C(u) = sum_{n=0}^p (-1)^n u^n / (2n)!      (cos x p  = C(x^2))
  S(u) = sum_{n=0}^p (-1)^n u^n / (2n+1)!    (sin x p  = x * S(x^2))

are coprime for any fixed p (checked here by the extended Euclidean
algorithm over exact rationals), which yields polynomials A, B with

  A * C + B * S = 1.

This identity is verified inside ImandraX by ground evaluation (theorem
[cert16] in trig_zeros.iml) and refutes any common zero of the sections.

Usage: gen_trig_cert.py [p]   (default p = 16, the value of CERR.approx)
"""
import os
import sys

# enum.py in this directory would shadow the stdlib module of the same name
_here = os.path.dirname(os.path.abspath(__file__))
sys.path = [p for p in sys.path if os.path.abspath(p or ".") != _here]

from fractions import Fraction
from math import factorial


def trim(a):
    while a and a[-1] == 0:
        a.pop()
    return a


def poly_divmod(a, b):
    a = a[:]
    q = [Fraction(0)] * max(0, len(a) - len(b) + 1)
    while len(a) >= len(b) and a:
        c = a[-1] / b[-1]
        d = len(a) - len(b)
        q[d] = c
        for i, bc in enumerate(b):
            a[i + d] -= c * bc
        trim(a)
    return q, a


def poly_sub(x, y):
    n = max(len(x), len(y))
    return trim([(x[i] if i < len(x) else 0) - (y[i] if i < len(y) else 0)
                 for i in range(n)])


def poly_mul(x, y):
    if not x or not y:
        return []
    r = [Fraction(0)] * (len(x) + len(y) - 1)
    for i, xi in enumerate(x):
        for j, yj in enumerate(y):
            r[i + j] += xi * yj
    return r


def poly_add(x, y):
    n = max(len(x), len(y))
    return [(x[i] if i < len(x) else 0) + (y[i] if i < len(y) else 0)
            for i in range(n)]


def bezout(a, b):
    r0, r1 = a[:], b[:]
    s0, s1 = [Fraction(1)], []
    t0, t1 = [], [Fraction(1)]
    while r1:
        q, r = poly_divmod(r0, r1)
        r0, r1 = r1, r
        s0, s1 = s1, poly_sub(s0, poly_mul(q, s1))
        t0, t1 = t1, poly_sub(t0, poly_mul(q, t1))
    return r0, s0, t0


def iml_lit(fr):
    n, d = fr.numerator, fr.denominator
    s = f"{abs(n)}.0"
    if d != 1:
        s = f"{s} /. {d}.0"
    if n < 0:
        s = f"(-. ({s}))"
    return s


def iml_list(name, coeffs):
    body = ";\n   ".join(iml_lit(x) for x in coeffs)
    return f"let {name} : real list =\n  [{body}]\n"


def main():
    p = int(sys.argv[1]) if len(sys.argv) > 1 else 16
    C = [Fraction((-1) ** n, factorial(2 * n)) for n in range(p + 1)]
    S = [Fraction((-1) ** n, factorial(2 * n + 1)) for n in range(p + 1)]
    g, A, B = bezout(C, S)
    assert len(g) == 1, f"sections not coprime at p={p}?!"
    A = [x / g[0] for x in A]
    B = [x / g[0] for x in B]
    chk = poly_add(poly_mul(A, C), poly_mul(B, S))
    assert chk[0] == 1 and all(v == 0 for v in chk[1:]), "certificate check failed"
    print(iml_list(f"cos_coeffs{p}", C))
    print(iml_list(f"sin_coeffs{p}", S))
    print(iml_list(f"bezout_a{p}", A))
    print(iml_list(f"bezout_b{p}", B))


if __name__ == "__main__":
    main()
