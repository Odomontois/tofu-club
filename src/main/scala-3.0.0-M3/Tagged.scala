import io.circe._
import io.circe.syntax.given
import io.circe.parser._

enum Expr:
  case Plus(left: Expr, right: Expr)
  case Mul(left: Expr, right: Expr)
  case Const(v: BigInt)

import Expr._

// (1 + 2)  * (3 + 4)
val testExpr = 
  Expr.Mul(
    Expr.Plus(
      Expr.Const(1),
      Expr.Const(2)
    ),
    Expr.Plus(
      Expr.Const(3),
      Expr.Const(4),
    )
  )

given Encoder[Expr] with
  def apply(expr: Expr): Json = expr match 
      case Const(c) => c.asJson
      case Plus(e1, e2) => Map("plus" -> Vector(e1.asJson, e2.asJson)).asJson
      case Mul(e1, e2) => Map("mul" -> Vector(e1.asJson, e2.asJson)).asJson

given Decoder[Expr] with
  def apply(c: HCursor): Decoder.Result[Expr] = 
    c.value.asNumber.flatMap(num => num.toBigInt.map(Const(_)))
    .orElse(
      c.downField("plus")
      .as[Vector[Expr]]
      .toOption.collect{
        case Vector(expr1, expr2) => Plus(expr1, expr2)
      }
    ).orElse(
      c.downField("mul")
      .as[Vector[Expr]]
      .toOption.collect{
        case Vector(expr1, expr2) => Mul(expr1, expr2)
      }
    ).toRight(DecodingFailure("it's a wrong format, bro", Nil))

@main def check = 
  println(testExpr)
  val json = testExpr.asJson
  println(json.spaces2)
  println(json.as[Expr])

enum Expr2:
  case Plus(left: Expr2, right: Expr2)
  case Mul(left: Expr2, right: Expr2)
  case Const(v: BigInt)
  case Var(name: String)

type Expr2A = Either[Expr, String]

// (x + 2) * y
val exprX2Y = Expr2.Mul(Expr2.Plus(Expr2.Var("x") , Expr2.Const(2)), Expr2.Var("y"))

enum Expr3:
  case Plus(left: Expr3, right: Expr3)
  case Mul(left: Expr3, right: Expr3)
  case Const(v: BigInt)
  case Concat(left: Expr3, right: Expr3)

enum Expr4:
  case Plus(left: Expr4, right: Expr4)
  case Mul(left: Expr4, right: Expr4)
  case Const(v: BigInt)
  case Concat(left: Expr4, right: Expr4)
  case Var(name: String)