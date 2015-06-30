package scala.virtualization.lms
package common

import language.dynamics
import language.experimental.macros

import scala.collection.mutable
import scala.reflect.macros.whitebox
import scala.reflect.SourceContext

object CodeCache {
  type ID = Long
  val guard: mutable.Map[ID, (Any, Any, Any)] = mutable.Map.empty
  val code: mutable.Map[(ID, List[Boolean]), Any] = mutable.Map.empty
}


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
  protected def dunit[T:Manifest](x: T): Rep[T] // TODO replace with holes!

  // holes
  def hole[T: Manifest](v: T, nr: Int): Dyn[T] = Both(v, holeImpl[T](nr))

  protected def holeImpl[T: Manifest](nr: Int): Rep[T]

}

trait DynamicExp extends internal.Expressions with DynamicBase with BaseExp {
  val UID: Long
  case class DConst[+T:Manifest](x: T) extends Exp[T]
  protected def dunit[T:Manifest](x: T): Rep[T] = DConst(x)
  val holes = mutable.HashMap.empty[Int, Sym[_]]
  case class Hole[T: Manifest](id: Int) extends Def[T]
  def holeImpl[T: Manifest](nr: Int): Rep[T] = {
    if (holes.contains(nr)) holes(nr).asInstanceOf[Rep[T]]
    else {
      val holeSym = toAtom(Hole[T](nr))
      holes += (nr -> holeSym.asInstanceOf[Sym[_]])
      holeSym
    }
  }
  def orderedHoles: List[Sym[_]] = holes.toList.sortBy(_._1).map(_._2)

  case class Lookup[T](uid: Long, syms: List[Sym[_]], decisions: List[Boolean]) extends Def[T]
  case class Recompile[T](uid: Long, syms: List[Sym[_]], decisions: List[Boolean]) extends Def[T]
  def emitLookup[T: Manifest](x: List[Sym[_]], decisions: List[Boolean]): Rep[T] = Lookup[T](UID, x, decisions)
  def emitRecompile[T: Manifest](x: List[Sym[_]], decisions: List[Boolean]): Rep[T] = Recompile[T](UID, x, decisions)

}

trait DynamicGen extends internal.GenericCodegen {
  val IR: DynamicExp
  import IR._
  override def quote(x: Exp[Any]): String = x match {
    case DConst(c) => quote(Const(c))
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Hole(id) => quote(sym)
    case Lookup(id, syms, decisions) =>
      emitValDef(sym, s"""scala.virtualization.lms.common.CodeCache.code(($id, ${decisions.toArray.mkString("List(", ",", ")")})).asInstanceOf[(${syms.map(_.tp.toString).mkString("", ", ", "")}) => ${sym.tp.toString}].apply(${syms.map(quote).mkString("",",", "")})""")
    case Recompile(id, syms, decisions) =>
      stream.println(s"val (recompile: (${syms.map(_.tp.toString).mkString("", ", ", "")} => ${syms.map(_.tp.toString).mkString("", ", ", "")} => ${sym.tp.toString}), _, _) = scala.virtualization.lms.common.CodeCache.guard($UID)")
      emitValDef(sym, s"recompile(${syms.map(quote).mkString("",",", "")})(${syms.map(quote).mkString("",",", "")})")
    case _ => super.emitNode(sym, rhs)
  }
}



trait DynIfThenElse extends DynamicBase with IfThenElse {
  trait Node
  case class DecisionNode(var cond: Boolean, tree: Rep[Boolean], semanticPreserving: Boolean, var left: (Node, Boolean), var right: (Node, Boolean)) extends Node
  case object Leaf extends Node
  var semanticPreserving = false
  var root: Node = Leaf
  var currentNode: Option[DecisionNode] = None

  private def makeDecision(cond: Dyn[Boolean]): Unit = {
    // find if we were already here
    val decision = DecisionNode(cond.static, cond.dynamic, semanticPreserving, (Leaf, !cond.static), (Leaf, cond.static))
    currentNode match {
      case None if root == Leaf => // first run or no decisions
        root = decision
        currentNode = Some(decision)
      case None => // root is not a leaf (reuse it)
        val tmpRoot = root.asInstanceOf[DecisionNode]
        currentNode = Some(tmpRoot)
        tmpRoot.cond = cond.static
        if (tmpRoot.right._2 == false && cond.static == true) tmpRoot.right = (tmpRoot.right._1, true)
        if (tmpRoot.left._2 == false && cond.static == false) tmpRoot.left = (tmpRoot.left._1, true)
        println("Root case: " + root)
      case Some(cn@DecisionNode(d, _, _, (l, lvisited), (r,rvisited))) =>
        if (d) r  match {
          case Leaf => // never been there
            cn.right = (decision, true)
            currentNode = Some(decision)
          case node: DecisionNode =>
            node.cond = d
            currentNode = Some(node)
        } else l match {
          case Leaf => // never been there
            cn.left = (decision, true)
            currentNode = Some(decision)
          case node: DecisionNode =>
            node.cond = d
            currentNode = Some(node)
        }
    }
  }

  def __ifThenElse[T:Manifest](cond: Dyn[Boolean], thenp: => Dyn[T], elsep: => Dyn[T])(implicit pos: SourceContext): Dyn[T] = {
    makeDecision(cond)
    if (cond.static) thenp else elsep
  }

  def __ifThenElse[T:Manifest](cond: Dyn[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext): Rep[T] = {
    println("Making decision: " + cond)
    makeDecision(cond)
    if (cond.static) thenp else elsep
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
