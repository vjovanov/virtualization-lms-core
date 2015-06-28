package scala.virtualization.lms
package common

import language.dynamics
import language.experimental.macros
import scala.reflect.macros.whitebox
import scala.reflect.SourceContext

trait DynamicBase extends Base {
  type Dyn[+T] = Both[T]

  case class Both[+T](static: T , dynamic: Rep[T]){
    /**
     * Redirect the invocation to both values at the same time producing new both on the way.
     * @return if all arguments are not Dyn[T] or T reduce back to Rep[T].
     */
    def applyDynamic(name: String)(args: Any*): Any = macro DynamicMacro.applyDynamicMacro
  }

  // TODO decide what should be the constant!
  def lift[T: Manifest](v: T): Dyn[T] = Both(v, dunit(v)(manifest[T]))
  def unlift[T](v: Dyn[T]): T = v.static
  protected def dunit[T:Manifest](x: T): Rep[T]

}

trait DynamicExp extends internal.Expressions with DynamicBase with BaseExp {
  case class DConst[+T:Manifest](x: T) extends Exp[T]
  protected def dunit[T:Manifest](x: T): Rep[T] = DConst(x)
}

trait DynamicGen extends internal.GenericCodegen {
  val IR: DynamicExp
  import IR._
  override def quote(x: Exp[Any]): String = x match {
    case DConst(c) => quote(Const(c))
    case _ => super.quote(x)
  }
}



trait DynIfThenElse extends DynamicBase with IfThenElse {
  val decisions: collection.mutable.ArrayBuffer[Rep[Boolean]] = collection.mutable.ArrayBuffer.empty
  def __ifThenElse[T:Manifest](cond: Dyn[Boolean], thenp: => Dyn[T], elsep: => Dyn[T])(implicit pos: SourceContext): Dyn[T] = {
    // here we need to record the dynamic part
    decisions += cond.dynamic
    if ((cond.static: Boolean)) thenp else elsep
  }

  def __ifThenElse[T:Manifest](cond: Dyn[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext): Rep[T] = {
    decisions += cond.dynamic
    if ((cond.static: Boolean)) thenp else elsep
  }

   // HACK -- bug in scala-virtualized
  override def __ifThenElse[T](cond: =>Boolean, thenp: => T, elsep: => T) = cond match {
    case true => thenp
    case false => elsep
  }

}



/**
 * Companion object of [[DynamicMacro]].
 *
 * This object contains the macro implementations of the
 * [[DynamicMacro]] trait.
 */
object DynamicMacro {

  /** Boilerplate type parameter extraction for `applyDynamic` macro. */
  def applyDynamicMacro(c: whitebox.Context)(
    name: c.Expr[String])(args: c.Expr[Any]*): c.Tree = {
    import c.universe._
    val q"$receiver.applyDynamic(...$args)" = c.macroApplication
    val Literal(Constant(mName: String)) = args.head.head
    val allArgsDynamic = args.tail.forall(_.forall{ arg => arg.tpe match {
      case TypeRef(_, tname, _ :: Nil) if tname.toString == "type Dyn" =>  true
      case TypeRef(_, tname, _ :: Nil) if tname.toString == "type Rep" =>  false
      case _ => true
    }})
    val x = TermName(c.freshName())
    if (allArgsDynamic)
      q"""
        val $x = $receiver
        Both($x.static.${TermName(mName).encodedName.toTermName}(...${args.tail.map(_.map(x => q"$x.static"))}), $x.dynamic.${TermName(mName).encodedName.toTermName}(...${args.tail.map(_.map(x => q"$x.dynamic"))}))
      """
    else {
      q"$receiver.dynamic.${TermName(mName).encodedName.toTermName}(...${args.tail})"
    }

  }


}
