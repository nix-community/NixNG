{ ... }:
let
  entry = type: path: mode: user: group: age: argument: {
    inherit
      type
      path
      mode
      user
      group
      age
      argument
      ;
  };
in
{
  _ = null;

  # for r in ["r", "_"]:
  # for w in ["w", "_"]:
  #   for x in ["x", "_"]:
  #     print(f"{r}{w}{x} = \"{r.replace("_", "-")}{w.replace("_", "-")}{x.replace("_", "-")}\";")
  rwx = "rwx";
  rw_ = "rw-";
  r_x = "r-x";
  r__ = "r--";
  _wx = "-wx";
  _w_ = "-w-";
  __x = "--x";
  ___ = "---";

  f = entry "f";
  fp = entry "f+";
  w = entry "w";
  wp = entry "w+";
  d = entry "d";
  D = entry "D";
  e = entry "e";
  v = entry "v";
  q = entry "q";
  Q = entry "Q";
  p = entry "p";
  pp = entry "p+";
  L = entry "L";
  Lp = entry "L+";
  c = entry "c";
  cp = entry "c+";
  b = entry "b";
  bp = entry "b+";
  C = entry "C";
  Cp = entry "C+";
  x = entry "x";
  X = entry "X";
  r = entry "r";
  R = entry "R";
  z = entry "z";
  Z = entry "Z";
  t = entry "t";
  T = entry "T";
  h = entry "h";
  H = entry "H";
  a = entry "a";
  ap = entry "a+";
  A = entry "A";
  Ap = entry "A+";
}
