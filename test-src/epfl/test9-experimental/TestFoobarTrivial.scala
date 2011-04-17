package scala.virtualization.lms
package epfl
package test9

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

trait FooBarTrivial {
  
  class Exp[+T](val str: String)

  var nVars = 0
  def fresh[T] = { nVars += 1; new Exp[T]("x" + (nVars -1)) }
  
  def id(x: String): String = x
  
  implicit def toAtom[T](rhs: String): Exp[T] = {
    val s = fresh[T]
    println("val "+s.str+" = "+rhs)
    s
  }

  def reifyBlock[T](e: =>Exp[T]): Exp[T] = {
    println("{")
    val r = e
    println(r.str)
    print("}")
    r
  }
  
  // --- client dsl

  def ifThenElse[T:Manifest](c: Exp[Boolean], a: =>Exp[T], b: =>Exp[T]): Exp[T] = {
    val s = fresh[T]
    print("val "+s.str+" = if ("+c.str+") ")
    reifyBlock(a)
    print(" else ")
    reifyBlock(b)
    println
    s
  }
  
  def whileDo(c: =>Exp[Boolean], b: =>Exp[Unit]): Exp[Unit] = {
    val s = fresh[Unit]
    print("val "+s.str+" = while (")
    reifyBlock(c)
    print(")")
    reifyBlock(b)
    println
    s
  }
  

  def varInit[T:Manifest](x: Exp[T]): Exp[T] = {
    val s = fresh[T]
    println("var "+s.str+" = "+x.str)
    s
  }
  
  def varAssign[T](v: Exp[T], x: Exp[T]): Exp[Unit] = id("("+v.str+" = "+x.str+")")
  
  def plus[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = toAtom(a.str+" + "+b.str)
  def minus[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = toAtom(a.str+" - "+b.str)
  def times[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = toAtom(a.str+" * "+b.str)
  def div[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = toAtom(a.str+" / "+b.str)
  
  def equal[T,U](a: Exp[T], b: Exp[U]) = toAtom(a.str+" == "+b.str)
  def notEqual[T,U](a: Exp[T], b: Exp[U]) = toAtom(a.str+" != "+b.str)
  
  def lessThan[T:Ordering](a: Exp[T], b: Exp[T]) = toAtom(a.str+" < "+b.str)
  def lessThanEqual[T:Ordering](a: Exp[T], b: Exp[T]) = toAtom(a.str+" <= "+b.str)
  def greaterThan[T:Ordering](a: Exp[T], b: Exp[T]) = toAtom(a.str+" > "+b.str)
  def greaterThanEqual[T:Ordering](a: Exp[T], b: Exp[T]) = toAtom(a.str+" >= "+b.str)
  
}


trait FooBarLiftTrivial extends EmbeddedControls with FooBarTrivial with OverloadHack {
  
  type Rep[+T] = Exp[T]
  
  implicit def unit[T:Manifest](x: T): Rep[T] = new Exp(x.toString)
  
  implicit def numericOps[T:Manifest:Numeric](x: Exp[T]) = new {
    def +(y: Exp[T]) = plus(x,y)
    def -(y: Exp[T]) = minus(x,y)
    def *(y: Exp[T]) = times(x,y)
    def /(y: Exp[T]) = div(x,y)
  }
/*  
  def infix_+[T:Manifest:Numeric](a: Exp[T], b: Exp[T]) = plus(a,b)
  def infix_-[T:Manifest:Numeric](a: Exp[T], b: Exp[T]) = minus(a,b)
  def infix_*[T:Manifest:Numeric](a: Exp[T], b: Exp[T]) = times(a,b)
  def infix_/[T:Manifest:Numeric](a: Exp[T], b: Exp[T]) = div(a,b)
*/  

  implicit def orderingOps[T:Manifest:Ordering](x: Exp[T]) = new {
    def <(y: Exp[T]) = lessThan(x,y)
    def <=(y: Exp[T]) = lessThanEqual(x,y)
    def >(y: Exp[T]) = greaterThan(x,y)
    def >=(y: Exp[T]) = greaterThanEqual(x,y)
  }

  
  def __newVar[T:Manifest](x: T) = varInit(unit(x))
  def __newVar[T](x: Rep[T])(implicit o: Overloaded1, mT: Manifest[T]) = varInit(x)
  
  def __assign[T:Manifest](lhs: Rep[T], rhs: T) = varAssign(lhs, unit(rhs))
  def __assign[T](lhs: Rep[T], rhs: Rep[T])(implicit o: Overloaded1, mT: Manifest[T]) = varAssign(lhs, rhs)
  
  def __ifThenElse[T:Manifest](cond: => Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = ifThenElse(cond, thenp, elsep)
  override def __ifThenElse[T](cond: => Boolean, thenp: => T, elsep: => T): T = cond match { case true => thenp case false => elsep }
  
  def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) = whileDo(cond, body)
  
  def __equal[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = equal(a,b)
  def __equal[A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = equal(a, unit(b))
  def __equal[A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = equal(unit(a), b)

  def infix_![A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = notEqual(a,b)
  def infix_![A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = notEqual(a, unit(b))
  def infix_![A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = notEqual(unit(a), b)
  
}


class TestFoobarTrivial extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test9-"
  
  trait DSL extends FooBarTrivial with FooBarLiftTrivial {
    def test(x: Rep[Int]): Rep[Any]
  }
  trait Impl extends DSL {

    println("object FooBar extends (Int => Any) {")
    val x = fresh[Int]
    print("def apply("+x.str+": Int) = ")
    reifyBlock(test(x))
    println
    println("}")
  }
  
  def testFoobar1 = {
    withOutFile(prefix+"foobartriv1") {
      trait Prog extends DSL {
        def power(b: Rep[Double], x: Int): Rep[Double] = 
          if (x == 0) 1.0
          else if ((x&1) == 0) { val y = power(b, x/2); y * y }
          else b * power(b, x - 1)
          
        def test(x: Rep[Int]) = {
          power(x.asInstanceOf[Rep[Double]], 8)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"foobartriv1")
  }

}
