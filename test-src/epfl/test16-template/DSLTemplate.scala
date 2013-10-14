package scala.virtualization.lms
package epfl
package test16

import common._
import test1._
import test7._
import test8.{ArrayMutation,ArrayMutationExp,ScalaGenArrayMutation,OrderingOpsExpOpt}

import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext
import scala.virtualization.lms.common.{ObjectOpsExp,WorklistTransformer}

// TODO we need to compose the list of functionalities (manually probably?)
trait DSLOps extends LiftNumeric with NumericOps with PrimitiveOps with ArrayOps with RangeOps with BooleanOps with LiftVariables with IfThenElse with Print {
  // <template> List of IR nodes
   def ifOp[T: Manifest](c: Rep[Boolean], t: Rep[T], e: Rep[T]): Rep[T]
}

// TODO we need to compose the list of functionalities (manually probably?)
trait DSLExp extends DSLOps with LMSTransform with ArrayOpsExpOpt with NumericOpsExpOpt with PrimitiveOpsExp with OrderingOpsExpOpt with BooleanOpsExp
    with EqualExpOpt with VariablesExpOpt with RangeOpsExp with StaticDataExp with IfThenElseExpOpt with PrintExp {
  val myLoweringPhase = new LoweringTransformer
  appendTransformer(myLoweringPhase)


  // <template> List of IR nodes
  case class IfOperation[T: Manifest](c: Rep[Boolean], t: Rep[T], e: Rep[T]) extends Def[T]


  // <template> List of constructors
  def ifOp[T: Manifest](c: Rep[Boolean], t: Rep[T], e: Rep[T]): Rep[T] = {
    IfOperation(c, t, e)
  }

  override def onCreate[A:Manifest](s: Sym[A], d: Def[A]) = (d match {
    // <template> implementation for each operation
    case IfOperation(c, t, e)   => s.atPhase(myLoweringPhase) {
      if(c) t else e
    }
    case _ => super.onCreate(s,d)
  }).asInstanceOf[Exp[A]]
}
