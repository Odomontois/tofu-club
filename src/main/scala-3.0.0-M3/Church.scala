type ExprC = 
  [A] => 
    ((A, A) => A) => 
    ((A, A) => A) => 
    (BigInt => A) => 
    A     

trait ExprC2 {
  def apply[A](plus : (A, A) => A, mul: (A, A) => A, const: BigInt => A): A
}

object ExprC:
  def Plus(left: ExprC, right: ExprC): ExprC = 
    [A] => (ifPlus : ((A, A) => A)) => (ifMul : ((A, A) => A)) => (ifConst :BigInt => A) => 
        ifPlus(left(ifPlus)(ifMul)(ifConst), right(ifPlus)(ifMul)(ifConst)) 

  def Mul(left: ExprC, right: ExprC): ExprC = 
    [A] => (ifPlus : ((A, A) => A)) => (ifMul : ((A, A) => A)) => (ifConst :BigInt => A) => 
        ifMul(left(ifPlus)(ifMul)(ifConst), right(ifPlus)(ifMul)(ifConst))

  def Const(v: BigInt): ExprC = 
    [A] => (ifPlus : ((A, A) => A)) => (ifMul : ((A, A) => A)) => (ifConst :BigInt => A) => 
       ifConst(v)

val testExprC: ExprC = 
    ExprC.Mul(
      ExprC.Plus(
        ExprC.Const(1),
        ExprC.Const(2)
      ),
    ExprC.Plus(
      ExprC.Const(3),
      ExprC.Const(4),
    )
  )

def showC(expr: ExprC): String = 
    expr[String]((l, r) => s"($l + $r)")((l, r) => s"($l * $r)")(c => c.toString)

def calcC(expr: ExprC): BigInt = 
    expr[BigInt]((l, r) => l + r)((l, r) => l * r)(c => c)

@main def checkChurch = 
    println(showC(testExprC))
    println(calcC(testExprC))
