import fastparse._, NoWhitespace._
import atp.ArithExpr._
import atp.ArithParser


Add(Mul(Add(Mul(Const(0),Var("x")),Const(1)),Const(3)),
Const(12)).show

ae"0"
ae"x1 + x3"
ae"3 x"
ae"(x1 + x2 + x3)  (1+2 + 3  x)"
ae"""(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) *
  (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)"""



