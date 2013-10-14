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


trait DSLGen extends ScalaGenPrimitiveOps with ScalaGenStaticData with ScalaGenOrderingOps with ScalaGenArrayOps with ScalaGenRangeOps
  with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenPrint /*with LivenessOpt*/ {
    val IR: DSLExp
    var transformers: List[WorklistTransformer{val IR: DSLGen.this.IR.type}] = Nil

    override def emitSource[A : Manifest](args: List[IR.Sym[_]], body: IR.Block[A], className: String, out: PrintWriter) = {
      var b: IR.Block[A] = body
      for (t <- transformers) {
        b = t.run(b)
      }
      super.emitSource(args, b, className, out)
    }
}

trait Impl extends DSLOps with DSLExp with Runner with CompileScala { self =>
    val codegen = new DSLGen {val IR: self.type = self}
    codegen.transformers = transformers
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
    run()
  }


  // test case input data
  trait Runner extends Compile {
    def test(x: Rep[Array[Int]]): Rep[Array[Int]]
    def run() {
      val f = compile(test)
      val v0 = Array(3, 1, 5, -2, 4)
      val v1 = f(v0)
      v1 foreach println
    }
  }

class DSLTemplate extends FileDiffSuite {
  // staged program implementations

  val prefix = "test-out/epfl/test16-"
  def testTemplate = {
    withOutFile(prefix+"template") {
      trait Prog extends DSLOps {
        def test(v: Rep[Array[Int]]) = {
          ifOp(unit(true), v, v)
        }
      }
      new Prog with DSLExp with Impl
    }
  }
}