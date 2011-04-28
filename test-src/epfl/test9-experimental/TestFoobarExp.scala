package scala.virtualization.lms
package epfl
package test9

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

trait FooBarExp {

  // -- expressions base

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def tp: Manifest[_] = manifest[T] //invariant position! but hey...
  }

  case class Const[+T:Manifest](x: T) extends Exp[T]

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    var sourceInfo = Thread.currentThread.getStackTrace // until we can get useful info out of the manifest
  }

  var nVars = 0
  def fresh[T:Manifest] = Sym[T] { nVars += 1; nVars -1 }

  // -- def and tp

  abstract class Def[+T] // operations (composite)

  case class TP[+T](sym: Sym[T], rhs: Def[T])

  var globalDefs: List[TP[Any]] = Nil

  def findDefinition[T](s: Sym[T]): Option[TP[T]] =
    globalDefs.find(_.sym == s).asInstanceOf[Option[TP[T]]]

  def findDefinition[T](d: Def[T]): Option[TP[T]] =
    globalDefs.find(_.rhs == d).asInstanceOf[Option[TP[T]]]

  def findOrCreateDefinition[T:Manifest](d: Def[T]): TP[T] =
    findDefinition[T](d).getOrElse {
      createDefinition(fresh[T], d)
    }

  def createDefinition[T](s: Sym[T], d: Def[T]): TP[T] = {
    val f = TP(s, d)
    globalDefs = globalDefs:::List(f)
    f
  }

  // --- api

  var scopeDefs: List[List[TP[Any]]] = Nil

  case class Block[T](stms: List[TP[Any]], e: Exp[T])

  protected implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] = {
    val f = createDefinition(fresh[T], d)
    scopeDefs = (scopeDefs.head:::List(f)) :: scopeDefs.tail
    f.sym
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).map(_.rhs)
      case _ =>
        None
    }
  }

  def reifyBlock[T](e: =>Exp[T]): Block[T] = {
    scopeDefs = Nil::scopeDefs
    val r = e
    val stms = scopeDefs.head
    scopeDefs = scopeDefs.tail
    Block(stms, r)
  }
  
  // --- analysis

  // forward dataflow analyis
  // alternative: effect abstraction. but: need to
  
  abstract class State
  
  def stateOrElse(a: State, b: State): State = a
  def stateAndAlso(a: State, b: State): State = a
  
  def stateInit: State = new State { }

  def abstractBlock[T](b: Block[T])(st: State): State = b match {
    case Block(stms, e) =>
      var st1 = st
      stms.foreach { t => 
        st1 = abstractNode(t.sym, t.rhs)(st1)
      }
      st1
  }

  def abstractNode[T](s: Sym[T], d: Def[T])(st: State): State = d match {
    case IfThenElse(c,a,b) => 
      stateOrElse(abstractBlock(a)(st),abstractBlock(b)(st))
    case WhileDo(c,b) =>
      var st0 = abstractBlock(c)(st)
      var st1 = st0
      do {
        st0 = st1
        st1 = abstractBlock(b)(st1)
        st1 = abstractBlock(c)(st1)
        st1 = stateOrElse(st0,st1)
      } while (st1 != st0)
      st1
      
    case _ =>
      st
  }
  
  
  
  



  // --- codegen

  var nesting = 0
  var indent = true

  def quote(s: Any): String = s match {
    case s @ Sym(id) => "x" + id
    case Const(c) => c.toString
  }

  def emitValDef[T](s: Sym[T], rhs: String, more: Boolean = false) = {
    emitPlain("val " + quote(s) + " = " + rhs, more)
  }

  def emitPlain(s: String, more: Boolean = false) = {
    if (indent) print(" " * (nesting * 2))
    if (more) print(s) else println(s)
    indent = !more
  }

  def emitNode[T](s: Sym[T], d: Def[T]): Unit = d match {
    case IfThenElse(c,a,b) => 
      emitValDef(s, "if (" + quote(c) + ") ", true)
      emitBlock(a, true)
      emitPlain(" else ", true)
      emitBlock(b)
    case VarInit(x) => 
      emitPlain("var " + quote(s) + " = " + quote(x))
    case VarAssign(v,x) => 
      emitValDef(s, "(" + quote(v) + " = " + quote(x) + ")")
    case WhileDo(c,b) =>
      emitValDef(s, "while (", true)
      emitBlock(c, true)
      emitPlain(") ", true)
      emitBlock(b)
    case _ =>
      emitValDef(s, "UNKNOWN: " + d)
  }

  def emitBlock[T](a: Block[T], more: Boolean = false) = a match {
    case Block(stms, e) =>
      emitPlain("{"/*}*/, false)
      nesting += 1
      stms foreach { t => emitNode(t.sym, t.rhs) }
      emitPlain(quote(e))
      nesting -= 1
      emitPlain(/*{*/"}", more)
  }


  // --- client dsl

  case class IfThenElse[T](c: Exp[Boolean], a: Block[T], b: Block[T]) extends Def[T]
  case class WhileDo(c: Block[Boolean], b: Block[Unit]) extends Def[Unit]

  def ifThenElse[T:Manifest](c: Exp[Boolean], a: =>Exp[T], b: =>Exp[T]): Exp[T] = {
    IfThenElse(c, reifyBlock(a), reifyBlock(b))
  }
  
  def whileDo(c: =>Exp[Boolean], b: =>Exp[Unit]): Exp[Unit] = {
    WhileDo(reifyBlock(c), reifyBlock(b))
  }
  

  case class VarInit[T](x: Exp[T]) extends Def[T]
  case class VarAssign[T](v: Exp[T], x: Exp[T]) extends Def[Unit]

  def varInit[T:Manifest](x: Exp[T]): Exp[T] = VarInit(x)
  def varAssign[T](v: Exp[T], x: Exp[T]): Exp[Unit] = VarAssign(v, x)
  
  case class Plus[T](a: Exp[T], b: Exp[T]) extends Def[T]
  case class Minus[T](a: Exp[T], b: Exp[T]) extends Def[T]
  case class Times[T](a: Exp[T], b: Exp[T]) extends Def[T]
  case class Div[T](a: Exp[T], b: Exp[T]) extends Def[T]
  
  def plus[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = Plus(a,b)
  def minus[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = Minus(a,b)
  def times[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = Times(a,b)
  def div[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = Div(a,b)
  
  case class Equal[T](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  case class NotEqual[T](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  
  def equal[T,U](a: Exp[T], b: Exp[U]) = Equal(a,b)
  def notEqual[T,U](a: Exp[T], b: Exp[U]) = NotEqual(a,b)
  
  case class LessThan[T:Ordering](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  case class LessThanEqual[T:Ordering](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  case class GreaterThan[T:Ordering](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  case class GreaterThanEqual[T:Ordering](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  
  def lessThan[T:Ordering](a: Exp[T], b: Exp[T]) = LessThan(a,b)
  def lessThanEqual[T:Ordering](a: Exp[T], b: Exp[T]) = LessThanEqual(a,b)
  def greaterThan[T:Ordering](a: Exp[T], b: Exp[T]) = GreaterThan(a,b)
  def greaterThanEqual[T:Ordering](a: Exp[T], b: Exp[T]) = GreaterThanEqual(a,b)

}


trait FooBarOptExp extends FooBarExp {
  //---
}


trait FooBarLiftExp extends EmbeddedControls with FooBarExp with OverloadHack {
  
  type Rep[+T] = Exp[T]
  
  implicit def unit[T:Manifest](x: T): Rep[T] = Const(x)
  
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
  override def __ifThenElse[T](cond: => Boolean, thenp: => T, elsep: => T): T = cond match { // HACK -- bug in scala-virtualized
    case true => thenp
    case false => elsep
  }
  
  def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) = whileDo(cond, body)
  
  def __equal[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = equal(a,b)
  def __equal[A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = equal(a, unit(b))
  def __equal[A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = equal(unit(a), b)

  def infix_![A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = notEqual(a,b)
  def infix_![A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = notEqual(a, unit(b))
  def infix_![A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = notEqual(unit(a), b)
  
}


class TestFoobarExp extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test9-"
  
  trait DSL extends FooBarExp with FooBarLiftExp with FooBarOptExp {
    def test(x: Rep[Int]): Rep[Any]
  }
  trait Impl extends DSL {

    val x = fresh[Int]
    val y = reifyBlock(test(x))
    
    
    abstractBlock(y)(stateInit)
    
    
    emitPlain("object FooBar extends (Int => Any) {"/*}*/)
    emitPlain("def apply("+quote(x)+": Int) = ", true)
    emitBlock(y)
    emitPlain(/*{*/"}")
  }
  
  def testFoobar1 = {
    withOutFile(prefix+"foobarexp1") {
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
    assertFileEqualsCheck(prefix+"foobarexp1")
  }

  def testFoobar2 = {
    withOutFile(prefix+"foobarexp2") {
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = 0
          while (c < x) {
            c = c + 1
          }
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"foobarexp2")
  }
}