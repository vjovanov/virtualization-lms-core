package scala.virtualization.lms
package common

import java.io.PrintWriter

trait LiftBoolean {
  this: Base =>

  implicit def boolToBoolRep(b: Boolean) = unit(b)
}

trait BooleanOps extends Variables {
  implicit class BooleanOps(lhs: Rep[Boolean]) {
    def unary_! = {
      implicit val sc: SourceContext = new SourceContext{}
      boolean_negate(lhs)
    }
    def &&(rhs: Rep[Boolean]) = {
      implicit val sc: SourceContext = new SourceContext{}
      boolean_and(lhs, rhs)
    }
    def ||(rhs: Rep[Boolean]) = {
      implicit val sc: SourceContext = new SourceContext{}
      boolean_or(lhs, rhs)
    }
  }
  def boolean_negate(lhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_and(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
  def boolean_or(lhs: Rep[Boolean], rhs: Rep[Boolean])(implicit pos: SourceContext): Rep[Boolean]
}

trait BooleanOpsExp extends BooleanOps with EffectExp {
  case class BooleanNegate(lhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanAnd(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]
  case class BooleanOr(lhs: Exp[Boolean], rhs: Exp[Boolean]) extends Def[Boolean]

  def boolean_negate(lhs: Exp[Boolean])(implicit pos: SourceContext): Exp[Boolean] = BooleanNegate(lhs)
  def boolean_and(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext): Exp[Boolean] = BooleanAnd(lhs, rhs)
  def boolean_or(lhs: Exp[Boolean], rhs: Exp[Boolean])(implicit pos: SourceContext): Exp[Boolean] = BooleanOr(lhs, rhs)

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case BooleanNegate(x) => boolean_negate(f(x))
    case BooleanAnd(x, y) => boolean_and(f(x), f(y))
    case BooleanOr(x, y) => boolean_or(f(x), f(y))

    case Reflect(BooleanNegate(x), u, es) => reflectMirrored(Reflect(BooleanNegate(f(x)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(BooleanAnd(x, y), u, es) => reflectMirrored(Reflect(BooleanAnd(f(x), f(y)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case Reflect(BooleanOr(x, y), u, es) => reflectMirrored(Reflect(BooleanOr(f(x), f(y)), mapOver(f, u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenBooleanOps extends ScalaGenBase {
  val IR: BooleanOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b) => emitValDef(sym, "!" + quote(b))
    case BooleanAnd(lhs, rhs) => emitValDef(sym, quote(lhs) + " && " + quote(rhs))
    case BooleanOr(lhs, rhs) => emitValDef(sym, quote(lhs) + " || " + quote(rhs))
    case _ => super.emitNode(sym, rhs)
  }
}

