package scala.virtualization.lms
package common

import language.dynamics
import language.experimental.macros

import scala.reflect.macros.whitebox
import scala.reflect.SourceContext

import collection._
import collection.mutable
import collection.immutable
import collection.generic.CanBuildFrom
import java.util.concurrent.atomic.AtomicLong

object PrefixMap {
  def empty[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*): PrefixMap[T] = {
    val m: PrefixMap[T] = empty
    for (kv <- kvs) m += kv
    m
  }

  def newBuilder[T]: mutable.Builder[(String, T), PrefixMap[T]] =
    new mutable.MapBuilder[String, T, PrefixMap[T]](empty)

  implicit def canBuildFrom[T]
  : CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] =
    new CanBuildFrom[PrefixMap[_], (String, T), PrefixMap[T]] {
      def apply(from: PrefixMap[_]) = newBuilder[T]
      def apply() = newBuilder[T]
    }
}

class PrefixMap[T]
  extends mutable.Map[String, T]
  with mutable.MapLike[String, T, PrefixMap[T]] {

  var suffixes: immutable.Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes get s(0) flatMap (_.get(s substring 1))

  def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  override def update(s: String, elem: T) =
    withPrefix(s).value = Some(elem)

  override def remove(s: String): Option[T] =
    if (s.isEmpty) { val prev = value; value = None; prev }
    else suffixes get s(0) flatMap (_.remove(s substring 1))

  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (chr +: s, v))

  def += (kv: (String, T)): this.type = { update(kv._1, kv._2); this }

  def -= (s: String): this.type  = { remove(s); this }

  override def empty = new PrefixMap[T]
}


object CodeCache {
  type ID = Long
  val meta: mutable.Map[ID, (Any, Any)] = mutable.Map.empty
  val guards: mutable.Map[ID, Any] = mutable.Map.empty
  val code: mutable.Map[ID, PrefixMap[Any]] = mutable.Map.empty
  val counters: mutable.Map[(ID, String), AtomicLong] = mutable.Map.empty
  val minimum: mutable.Map[ID, AtomicLong] = mutable.Map.empty

  final val threshold = 0L

  final def recompileIfMRU[T](id: ID, decs: String, min: Long, x0: Any): T = {
    val counter = counters(id -> decs)
    val minimum = CodeCache.minimum(id).get
    if (counter.incrementAndGet() > minimum + threshold) recompileAndRun[T](id, x0)
    else (code(id) withPrefix decs.substring(0, decs.length - 1)).head._2.asInstanceOf[(Any) => T].apply(x0)
  }

  final def recompileIfMRU[T](id: ID, decs: String, min: Long, x0: Any, x1: Any): T = {
    val counter = counters(id -> decs)
    val minimum = CodeCache.minimum(id).get
    if (counter.incrementAndGet() > minimum + threshold) recompileAndRun[T](id, x0, x1)
    else (code(id) withPrefix decs.substring(0, decs.length - 1)).head._2.asInstanceOf[(Any, Any) => T].apply(x0, x1)
  }

  final def recompileIfMRU[T](id: ID, decs: String, min: Long, x0: Any, x1: Any, x2: Any): T = {
    val counter = counters(id -> decs)
    val minimum = CodeCache.minimum(id).get
    if (counter.incrementAndGet() > minimum + threshold) CodeCache.recompileAndRun[T](id, x0, x1, x2)
    else {
      (code(id) withPrefix decs.substring(0, decs.length - 1)).head._2.asInstanceOf[(Any, Any, Any) => T].apply(x0, x1, x2)
    }
  }

  final def recompileIfMRU[T](id: ID, decs: String, min: Long, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any): T = {
    val counter = counters(id -> decs)
    val minimum = CodeCache.minimum(id).get
    if (counter.incrementAndGet() > minimum + threshold) CodeCache.recompileAndRun[T](id, x0, x1, x2, x3, x4, x5, x6, x7, x8)
    else {
      (code(id) withPrefix decs).head._1.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any) => T].apply(x0, x1, x2, x3, x4, x5, x6, x7, x8)
    }
  }

  final def recompileIfMRU[T](id: ID, decs: String, min: Long, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any): T = {
    val counter = counters(id -> decs)
    val minimum = CodeCache.minimum(id).get
    if (counter.incrementAndGet() > minimum + threshold) CodeCache.recompileAndRun[T](id, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
    else {
      (code(id) withPrefix decs).head._1.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => T].apply(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
    }
  }

  final def run[T](id: ID, decs: String, x0: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any) => T](x0)
  }

  final def run[T](id: ID, decs: String, x0: Any, x1: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any, Any) => T](x0, x1)
  }

  final def run[T](id: ID, decs: String, x0: Any, x1: Any, x2: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any, Any, Any) => T](x0, x1, x2)
  }

  final def run[T](id: ID, decs: String, x0: Any, x1: Any, x2: Any, x3: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any, Any, Any, Any) => T](x0, x1, x2, x3)
  }

  final def run[T](id: ID, decs: String, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4)
  }

  final def run[T](id: ID, decs: String, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5)
  }

  final def run[T](id: ID, decs: String, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any, Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5, x6)
  }

  final def run[T](id: ID, decs: String, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5, x6, x7)
  }

  final def run[T](id: ID, decs: String, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5, x6, x7, x8)
  }

  final def run[T](id: ID, decs: String, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any): T = {
    val counter = counters(id -> decs).incrementAndGet()
    val atomicMinimum = CodeCache.minimum(id)
    val minimum = atomicMinimum.get
    if (counter == minimum + 1) atomicMinimum.incrementAndGet
    code(id)(decs).asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
  }

  final def recompileAndRun[T](id: ID, x0: Any): T = {
    meta(id)._1.asInstanceOf[(Any) => T](x0)
  }

  final def recompileAndRun[T](id: ID, x0: Any, x1: Any): T = {
    meta(id)._1.asInstanceOf[(Any, Any) => T](x0, x1)
  }

  final def recompileAndRun[T](id: ID, x0: Any, x1: Any, x2: Any): T = {
    meta(id)._1.asInstanceOf[(Any, Any, Any) => T](x0, x1, x2)
  }

  final def recompileAndRun[T](id: ID, x0: Any, x1: Any, x2: Any, x3: Any): T = {
    meta(id)._1.asInstanceOf[(Any, Any, Any, Any) => T](x0, x1, x2, x3)
  }

  final def recompileAndRun[T](id: ID, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any): T = {
    meta(id)._1.asInstanceOf[(Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4)
  }

  final def recompileAndRun[T](id: ID, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any): T = {
    meta(id)._1.asInstanceOf[(Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5)
  }

  final def recompileAndRun[T](id: ID, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any): T = {
    meta(id)._1.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5, x6)
  }

  final def recompileAndRun[T](id: ID, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any): T = {
    meta(id)._1.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5, x6, x7)
  }

  final def recompileAndRun[T](id: ID, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any): T = {
    meta(id)._1.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5, x6, x7, x8)
  }

  final def recompileAndRun[T](id: ID, x0: Any, x1: Any, x2: Any, x3: Any, x4: Any, x5: Any, x6: Any, x7: Any, x8: Any, x9: Any): T = {
    meta(id)._1.asInstanceOf[(Any, Any, Any, Any, Any, Any, Any, Any, Any, Any) => T](x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
  }

}



trait DynamicBase extends Base {
  type Dyn[+T] = Both[T]

  trait Static
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
  // TODO implicit evidence
  def holeRep[T: Manifest](v: T, nr: Int): Rep[T] = holeImpl[T](nr)

  protected def holeImpl[T: Manifest](nr: Int): Rep[T]

  def bitString(decs: Iterable[Boolean]): String = decs.map(if(_) "1" else "0").mkString("", "", "")
}

trait DynamicExp extends internal.Expressions with DynamicBase with BaseExp {
  val UID: Long
  case class DConst[+T:Manifest](x: T) extends Exp[T]
  protected def dunit[T:Manifest](x: T): Rep[T] = Const(x)
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
  case class RecompileMRU[T](uid: Long, syms: List[Sym[_]], decisions: List[Boolean], minimum: Long) extends Def[T]
  def emitLookup[T: Manifest](x: List[Sym[_]], decisions: List[Boolean]): Rep[T] = Lookup[T](UID, x, decisions)
  def emitRecompile[T: Manifest](x: List[Sym[_]], decisions: List[Boolean]): Rep[T] = Recompile[T](UID, x, decisions)
  def emitRecompileMRU[T: Manifest](x: List[Sym[_]], decisions: List[Boolean], minimum: Long): Rep[T] = RecompileMRU[T](UID, x, decisions, minimum)

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
      emitValDef(sym, s"""scala.virtualization.lms.common.CodeCache.run[${sym.tp.toString}]($id, "${bitString(decisions)}", ${syms.map(quote).mkString("",",", "")})""")
    case Recompile(id, syms, decisions) =>
      emitValDef(sym, s"""scala.virtualization.lms.common.CodeCache.recompileAndRun[${sym.tp.toString}]($UID, ${syms.map(quote).mkString("",",", "")})""")
    case RecompileMRU(id, syms, decisions, minimum) =>
      emitValDef(sym, s"""scala.virtualization.lms.common.CodeCache.recompileIfMRU[${sym.tp.toString}]($UID, "${bitString(decisions)}", $minimum, ${syms.map(quote).mkString("",",", "")})""")
    case _ => super.emitNode(sym, rhs)
  }
}



trait DynIfThenElse extends DynamicBase with IfThenElse {
  trait Node
  case class DecisionNode(
    tree: Rep[Boolean],
    semanticPreserving: Boolean,
    var left: Node,
    var right: Node) extends Node
  case class Leaf(decisions: Option[List[Boolean]]) extends Node

  var semanticPreserving = false
  def semanticPreserving[T](b: => T): T = {
    semanticPreserving = true
    val res = b
    semanticPreserving = false
    res
  }

  def emptyRoot = Leaf(Some(Nil))
  var root: Node = emptyRoot
  var parent: Option[DecisionNode] = None

  var decisions: List[Boolean] = Nil

  private def makeDecision(cond: Dyn[Boolean]): Unit = {
    decisions = decisions :+ cond.static
    // find if we were already here
    def leaf(c: Boolean) = if (c) Leaf(Some(decisions)) else Leaf(None)
    val decision = DecisionNode(cond.dynamic, semanticPreserving, leaf(!cond.static), leaf(cond.static))
    def updateNode(n: Node): Unit = n match {
      case node@DecisionNode(_, _, Leaf(None), _) if !cond.static => node.left = Leaf(Some(decisions))
      case node@DecisionNode(_, _, _, Leaf(None)) if cond.static => node.right = Leaf(Some(decisions))
      case _ =>
    }

    (parent, root) match {
      case (None, _: Leaf) => // first run or no decisions
        root = decision
        parent = Some(decision)

      case (None, root@DecisionNode(c, s, l, r)) => // root is not a leaf => reuse it
        parent = Some(root)
        updateNode(root)

      case (Some(cn@DecisionNode(_, _, l, r)), _) => //
        val c = decisions(decisions.length-2)
        if (c) r  match {
          case _: Leaf =>
            cn.right = decision
            parent = Some(decision)
          case node: DecisionNode =>
            parent = Some(node)
            updateNode(node)
        } else l match {
          case _: Leaf =>
            cn.left = decision
            parent = Some(decision)
          case node: DecisionNode =>
            parent = Some(node)
            updateNode(node)
        }
    }
  }

  def __ifThenElse[T:Manifest](cond: Dyn[Boolean], thenp: => Dyn[T], elsep: => Dyn[T])(implicit pos: SourceContext): Dyn[T] = {
    makeDecision(cond)
    if (cond.static) thenp else elsep
  }

  def __ifThenElse[T:Manifest](cond: Dyn[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext): Rep[T] = {2
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
