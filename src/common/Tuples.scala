package scala.virtualization.lms
package common

import scala.virtualization.lms.internal.GenericCodegen
import java.io.PrintWriter

trait TupleOps extends Base {

  implicit class Tuple2Ops[A: Manifest, B: Manifest](lhs: Rep[(A, B)]) {
    def _1: Rep[A] = {
        implicit def sc = (new SourceContext{})
        tuple2_get1(lhs)
    }
    def _2: Rep[B] = {
        implicit def sc = (new SourceContext{})
        tuple2_get2(lhs)
    }
  }
 implicit class Tuple3Ops[A:Manifest,B:Manifest,C:Manifest](lhs: Rep[(A, B, C)]) {
    def _1: Rep[A] = {
        implicit def sc = (new SourceContext{})
        tuple3_get1(lhs)
    }
    def _2: Rep[B] = {
        implicit def sc = (new SourceContext{})
        tuple3_get2(lhs)
    }
    def _3: Rep[C] = {
        implicit def sc = (new SourceContext{})
        tuple3_get3(lhs)
    }
  }
 implicit class Tuple4Ops[A:Manifest,B:Manifest,C:Manifest,D:Manifest](lhs: Rep[(A, B, C, D)]) {
    def _1: Rep[A] = {
        implicit def sc = (new SourceContext{})
        tuple4_get1(lhs)
    }
    def _2: Rep[B] = {
        implicit def sc = (new SourceContext{})
        tuple4_get2(lhs)
    }
    def _3: Rep[C] = {
        implicit def sc = (new SourceContext{})
        tuple4_get3(lhs)
    }
    def _4: Rep[D] = {
        implicit def sc = (new SourceContext{})
        tuple4_get4(lhs)
    }

  }

 def make_tuple2[A:Manifest,B:Manifest](t1: Rep[A], t2: Rep[B])(implicit pos: SourceContext) : Rep[(A,B)]
  object Tuple2 {
    def apply[A: Manifest, B: Manifest](_1: Rep[A], _2: Rep[B]) = {
      implicit def sc = (new SourceContext{})
      make_tuple2(_1, _2)
    }
  }
  def make_tuple3[A:Manifest,B:Manifest,C:Manifest](_1: Rep[A], _2: Rep[B], _3: Rep[C])(implicit pos: SourceContext) : Rep[(A,B,C)]
  object Tuple3 {
    def apply[A:Manifest,B:Manifest,C:Manifest](_1: Rep[A], _2: Rep[B], _3: Rep[C]) = {
      implicit def sc = (new SourceContext{})
      make_tuple3(_1, _2, _3)
    }
  }
  def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](_1: Rep[A], _2: Rep[B], _3: Rep[C], _4: Rep[D])(implicit pos: SourceContext) : Rep[(A,B,C,D)]
  object Tuple4 {
    def apply[A:Manifest,B:Manifest,C:Manifest,D:Manifest](_1: Rep[A], _2: Rep[B], _3: Rep[C], _4: Rep[D]) = {
      implicit def sc = (new SourceContext{})
      make_tuple4(_1, _2, _3, _4)
    }
  }

  def tuple2_get1[A:Manifest](t: Rep[(A,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple2_get2[B:Manifest](t: Rep[(_,B)])(implicit pos: SourceContext) : Rep[B]

  def tuple3_get1[A:Manifest](t: Rep[(A,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple3_get2[B:Manifest](t: Rep[(_,B,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple3_get3[C:Manifest](t: Rep[(_,_,C)])(implicit pos: SourceContext) : Rep[C]

  def tuple4_get1[A:Manifest](t: Rep[(A,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple4_get2[B:Manifest](t: Rep[(_,B,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple4_get3[C:Manifest](t: Rep[(_,_,C,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple4_get4[D:Manifest](t: Rep[(_,_,_,D)])(implicit pos: SourceContext) : Rep[D]

  def tuple5_get1[A:Manifest](t: Rep[(A,_,_,_,_)])(implicit pos: SourceContext) : Rep[A]
  def tuple5_get2[B:Manifest](t: Rep[(_,B,_,_,_)])(implicit pos: SourceContext) : Rep[B]
  def tuple5_get3[C:Manifest](t: Rep[(_,_,C,_,_)])(implicit pos: SourceContext) : Rep[C]
  def tuple5_get4[D:Manifest](t: Rep[(_,_,_,D,_)])(implicit pos: SourceContext) : Rep[D]
  def tuple5_get5[E:Manifest](t: Rep[(_,_,_,_,E)])(implicit pos: SourceContext) : Rep[E]
}

trait TupleOpsExp extends TupleOps with StructExpOpt {
  def make_tuple2[A:Manifest,B:Manifest](t1: Rep[A], t2: Rep[B])(implicit pos: SourceContext) : Rep[(A,B)] = struct(classTag[(A,B)], "_1" -> t1, "_2" -> t2)
  def make_tuple3[A:Manifest,B:Manifest,C:Manifest](_1: Rep[A], _2: Rep[B], _3: Rep[C])(implicit pos: SourceContext) : Rep[(A,B,C)] = struct(classTag[(A,B,C)], "_1" -> _1, "_2" -> _2, "_3" -> _3)
  def make_tuple4[A:Manifest,B:Manifest,C:Manifest,D:Manifest](_1: Rep[A], _2: Rep[B], _3: Rep[C], _4: Rep[D])(implicit pos: SourceContext) : Rep[(A,B,C,D)] = struct(classTag[(A,B,C,D)], "_1" -> _1, "_2" -> _2, "_3" -> _3, "_4" -> _4)


  def tuple2_get1[A:Manifest](t: Exp[(A,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple2_get2[B:Manifest](t: Exp[(_,B)])(implicit pos: SourceContext) = field[B](t, "_2")

  def tuple3_get1[A:Manifest](t: Exp[(A,_,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple3_get2[B:Manifest](t: Exp[(_,B,_)])(implicit pos: SourceContext) = field[B](t, "_2")
  def tuple3_get3[C:Manifest](t: Exp[(_,_,C)])(implicit pos: SourceContext) = field[C](t, "_3")

  def tuple4_get1[A:Manifest](t: Exp[(A,_,_,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple4_get2[B:Manifest](t: Exp[(_,B,_,_)])(implicit pos: SourceContext) = field[B](t, "_2")
  def tuple4_get3[C:Manifest](t: Exp[(_,_,C,_)])(implicit pos: SourceContext) = field[C](t, "_3")
  def tuple4_get4[D:Manifest](t: Exp[(_,_,_,D)])(implicit pos: SourceContext) = field[D](t, "_4")

  def tuple5_get1[A:Manifest](t: Exp[(A,_,_,_,_)])(implicit pos: SourceContext) = field[A](t, "_1")
  def tuple5_get2[B:Manifest](t: Exp[(_,B,_,_,_)])(implicit pos: SourceContext) = field[B](t, "_2")
  def tuple5_get3[C:Manifest](t: Exp[(_,_,C,_,_)])(implicit pos: SourceContext) = field[C](t, "_3")
  def tuple5_get4[D:Manifest](t: Exp[(_,_,_,D,_)])(implicit pos: SourceContext) = field[D](t, "_4")
  def tuple5_get5[E:Manifest](t: Exp[(_,_,_,_,E)])(implicit pos: SourceContext) = field[E](t, "_5")

  object Both { def unapply[T](x:T):Some[(T,T)] = Some((x,x)) }
}

trait TupleGenBase extends GenericCodegen with BaseGenStruct {
  val IR: TupleOpsExp

  override def remap[A](m: Manifest[A]) = m.erasure.getSimpleName match {

    case _ => super.remap(m)
  }
}

trait ScalaGenTupleOps extends ScalaGenBase with TupleGenBase with ScalaGenStruct { val IR: TupleOpsExp }