package scala.virtualization.lms
package epfl
package test14

import common._
import test1._

import scala.reflect.macros.Context
import language.experimental.macros
import util.OverloadHack
import java.io.{ PrintWriter, StringWriter, FileOutputStream }
import scala.reflect.SourceContext

/**
 * Test that shows type system problems with Rep[T] DSL Interface.
 */
trait RepDSL extends ScalaOpsPkg with LiftPrimitives with LiftBoolean with LiftString {

  /**
   * Definitions of varialbes get the wrong type if unit is not used.
   */
  def valVarDef(u: Rep[Unit]): Rep[Unit] = {
    def testMethod(a: Rep[Int], b: Rep[String], c: Rep[Boolean], d: Rep[Double]) = {
      System.out.println(a.toString)
      System.out.println(b.toString)
      System.out.println(c.toString)
      System.out.println(d.toString)
      print(a)
      print(b)
      print(c)
      print(d)
    } // constant val and var definitions

    // this works because implicit gets invoked on the method
    {
      val i = 1
      val s = "y"
      val b = true
      val d = 0.1

      testMethod(i, s, b, d)
    }

    // this would not work  because the variable will be of wrong type
    {
      var i = 1
      var s = "y"
      var b = true
      var d = 0.1
      val string = unit("string")
      
      testMethod(i, s, b, d)
      
      print(s + string)
    }

    // will this work ?
    {
      var i: Rep[Int] = 1
      var s: Rep[String] = "y"
      var b: Rep[Boolean] = true
      var d: Rep[Double] = 0.1

      testMethod(i, s, b, d)
      
    }
    ()
  }

  def tuples: Rep[Unit] = {
  }
}

class TestRepTypesErrors extends FileDiffSuite {

  val prefix = "test-out/epfl/test14-"

  def testStaging1 = {
    withOutFile(prefix + "type-errors") {
      new RepDSL with ScalaOpsPkgExp { self =>
        val codegen = new ScalaCodeGenPkg with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self }
        codegen.emitSource(valVarDef, "valVarDef", new PrintWriter(System.out))
      }
    }
    //assertFileEqualsCheck(prefix + "staging1")
  }

}
