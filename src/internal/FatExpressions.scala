package scala.virtualization.lms
package internal

trait FatExpressions extends Expressions {

  abstract class FatDef

  //case class ThinDef(rhs: Def[Any]) extends FatDef

  case class TTP(val lhs: List[Sym[Any]], val mhs: List[Def[Any]], val rhs: FatDef) extends Stm

 implicit class FatStmOps(val stm: Stm) extends StmOpsLike {
   override def lhs: List[Sym[Any]] = stm match {
     case TTP(lhs, mhs, rhs) => lhs
     case _ => super.lhs
   }

    override def rhs: Any = stm match { // clients use syms(e.rhs), boundSyms(e.rhs) etc.
      case TTP(lhs, mhs, rhs) => rhs
      case _ => super.rhs
    }

    override def defines[A](sym: Sym[A]): Option[Def[A]] = stm match {
      case TTP(lhs, mhs, rhs) => lhs.indexOf(sym) match { case idx if idx >= 0 => Some(mhs(idx).asInstanceOf[Def[A]]) case _ => None }
      case _ => super.defines(sym)
    }

    override def defines[A](rhs: Def[A]): Option[Sym[A]] = stm match {
       case TTP(lhs, mhs, rhs) => mhs.indexOf(rhs) match { case idx if idx >= 0 => Some(lhs(idx).asInstanceOf[Sym[A]]) case _ => None }
       case _ => super.defines(rhs)
    }
  }



  case class Combine(a: List[Exp[Any]]) extends Exp[Any] //TODO: get rid of. used by emitFatBlock

  case class Forward[A](x: Exp[A]) extends Def[A] // TODO: get rid of. used by SimplifyTransform

}
