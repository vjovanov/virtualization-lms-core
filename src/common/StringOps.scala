package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenerationFailedException}

trait LiftString {
  this: Base =>

  implicit def strToRepStr(s: String) = unit(s)
}

trait StringOps extends Variables with OverloadHack {
  // NOTE: if something doesn't get lifted, this won't give you a compile time error,
  //       since string concat is defined on all objects
  
  def infix_+(s1: Rep[String], s2: Rep[Any]) = string_plus(s1, s2)
  def infix_+(s1: Rep[String], s2: Var[Any])(implicit o: Overloaded1) = string_plus(s1, readVar(s2))
  def infix_+(s1: String, s2: Rep[Any])(implicit o: Overloaded4) = string_plus(unit(s1), s2)
  def infix_+(s1: String, s2: Var[Any])(implicit o: Overloaded5) = string_plus(unit(s1), readVar(s2))
  def infix_+(s1: Rep[Any], s2: Rep[String])(implicit o: Overloaded2) = string_plus(s1, s2)
  def infix_+(s1: Var[Any], s2: Rep[String])(implicit o: Overloaded3) = string_plus(readVar(s1), s2)
  def infix_+(s1: Rep[Any], s2: String)(implicit o: Overloaded6) = string_plus(s1, unit(s2))
  def infix_+(s1: Var[Any], s2: String)(implicit o: Overloaded7) = string_plus(readVar(s1), unit(s2))
  def infix_+(s1: Rep[String], s2: Var[Int])(implicit o: Overloaded8) = string_plus(s1, readVar(s2))
  def infix_+(s1: String, s2: Var[Int])(implicit o: Overloaded9) = string_plus(unit(s1), readVar(s2))

  def infix_trim(s: Rep[String]) = string_trim(s)
  def infix_split(s: Rep[String], separators: Rep[String]) = string_split(s, separators)
  def infix_endsWith(s: Rep[String], e: Rep[String]) = string_endsWith(s,e)
  def infix_charAt(s: Rep[String], i: Rep[Int]) = string_charAt(s,i)

  object String {
    def valueOf(a: Rep[Any]) = string_valueof(a)
  }

  def string_plus(s: Rep[Any], o: Rep[Any]): Rep[String]
  def string_trim(s: Rep[String]): Rep[String]
  def string_split(s: Rep[String], separators: Rep[String]): Rep[Array[String]]
  def string_valueof(d: Rep[Any]): Rep[String]
  def string_endsWith(s: Rep[String], e: Rep[String]): Rep[Boolean]
  def string_charAt(s: Rep[String], i: Rep[Int]): Rep[Char]
}

trait StringOpsExp extends StringOps with VariablesExp {
  case class StringPlus(s: Exp[Any], o: Exp[Any]) extends Def[String]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String]) extends Def[Array[String]]
  case class StringValueOf(a: Exp[Any]) extends Def[String]
  case class StringEndsWith(s: Exp[String], e: Exp[String]) extends Def[Boolean]
  case class StringCharAt(s: Exp[String], i: Exp[Int]) extends Def[Char]

  def string_plus(s: Exp[Any], o: Exp[Any]): Rep[String] = StringPlus(s,o)
  def string_trim(s: Exp[String]) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String]) : Rep[Array[String]] = StringSplit(s, separators)
  def string_valueof(a: Exp[Any]) = StringValueOf(a)
  def string_endsWith(s: Rep[String], e: Rep[String]) = StringEndsWith(s,e)
  def string_charAt(s: Exp[String], i: Exp[Int]) = StringCharAt(s,i)

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    case StringPlus(a,b) => string_plus(f(a),f(b))
    case StringEndsWith(s, e) => string_endsWith(f(s),f(e))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenStringOps extends ScalaGenBase {
  val IR: StringOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep) => emitValDef(sym, "%s.split(%s)".format(quote(s), quote(sep)))
    case StringValueOf(a) => emitValDef(sym, "java.lang.String.valueOf(%s)".format(quote(a)))
    case StringEndsWith(s, e) => emitValDef(sym, "%s.endsWith(%s)".format(quote(s), quote(e)))
    case StringCharAt(s,i) => emitValDef(sym, "%s.charAt(%i)".format(quote(s), quote(i)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenStringOps extends CudaGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringTrim(s) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringSplit(s, sep) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenStringOps extends OpenCLGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringTrim(s) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringSplit(s, sep) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case _ => super.emitNode(sym, rhs)
  }
}
trait CGenStringOps extends CGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym,"strcat(%s,%s);".format(quote(s1),quote(s2)))
    case StringTrim(s) => throw new GenerationFailedException("CGenStringOps: StringTrim not implemented yet")
    case StringSplit(s, sep) => throw new GenerationFailedException("CGenStringOps: StringSplit not implemented yet")
    case _ => super.emitNode(sym, rhs)
  }
}
