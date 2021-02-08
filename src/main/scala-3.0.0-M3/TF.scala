trait ExprA[A]:
  def plus(left: A, right: A): A
  def mul(left: A, right: A): A
  def const(v: BigInt): A

type ExprTF = [A] => ExprA[A] => A

object ExprTF:
  def Plus(left: ExprTF, right: ExprTF): ExprTF = 
    [A] => (alg: ExprA[A]) => alg.plus(left(alg), right(alg))

  def Mul(left: ExprTF, right: ExprTF): ExprTF = 
    [A] => (alg: ExprA[A]) => alg.mul(left(alg), right(alg))

  def Const(v: BigInt): ExprTF = 
    [A] => (alg: ExprA[A]) => alg.const(v)

val testExprTF: ExprTF = 
    ExprTF.Mul(
      ExprTF.Plus(
        ExprTF.Const(1),
        ExprTF.Const(2)
      ),
    ExprTF.Plus(
      ExprTF.Const(3),
      ExprTF.Const(4),
    )
  )

val ShowTF: ExprA[String] = new {
  def plus(l: String, r: String) = s"($l + $r)"
  def mul(l: String, r: String) = s"($l * $r)"
  def const(v: BigInt) = v.toString
}

val CalcTF: ExprA[BigInt] = new {
  def plus(l: BigInt, r: BigInt) = l + r
  def mul(l: BigInt, r: BigInt) = l * r
  def const(v: BigInt) = v
}

trait Vars[A]:
  def variable(name: String): A

trait Matrices[A]:
  def concat(x: A, y: A): A

type ExprVTF = [A] => (ExprA[A], Vars[A]) => A
type ExprMTF = [A] => (ExprA[A], Matrices[A]) => A
type ExprVMTF = [A] => (ExprA[A], Matrices[A], Vars[A]) => A

def enhanceWithVariables(e: ExprTF): ExprVTF = 
  [A] => (alg: ExprA[A], vars: Vars[A]) => e(alg)

def superEnhance(e: ExprTF): ExprVMTF = 
  [A] => (alg: ExprA[A], m: Matrices[A], vars: Vars[A]) => e(alg)

@main def checkTF = 
    println(testExprTF(ShowTF))
    println(testExprTF(CalcTF))

  



















// def Plus1[A](left: ExprTF, right: ExprTF): ExprA[A] => A = ??? 
// def Plus2(left: ExprTF) : [A] => (right: ExprTF) => ExprA[A] => A = ??? 
// def Plus3(left: ExprTF, right: ExprTF) : [A] =>  ExprA[A] => A = ??? 



// def foo: Int => String => Double = ???

// def foo1: String => Int => Double = y => x => foo(x)(y)

// def bar[A]: String => Int => List[A] => Option[A] = ???

// def barP: [A] => String => Int => List[A] => Option[A] = 
//   [A] => (x: String) => (y: Int) => (l: List[A]) => bar(x)(y)(l)

// def barP1: String => [A] => Int => List[A] => Option[A] = 
//   x => [A] => (y: Int) => (l: List[A]) => bar(x)(y)(l)

// def barP2: String => Int => [A] => List[A] => Option[A] = 
//   x => (y: Int) => [A] => (l: List[A]) =>  bar(x)(y)(l)
