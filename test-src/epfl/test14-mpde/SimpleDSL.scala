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
 * The trait that is required to be imported for the DSL to use mpde.
 */
trait MPDE extends Base { 
  import ch.epfl.lamp.mpde.MPDE
  
  def lift[T, R](block: T): T = macro MPDE.lift[T]  
}


/**
* Simple DSL Interface.
*/
trait SimpleDSL extends Base {

  // object method
  object Vector {
    def op1[T: Manifest](v: T): Rep[T] = ???

    def op1_rep[T: Manifest](v: Rep[T])(implicit sc: SourceContext): Rep[T]= ???
  }

  // simple method
  def op1[T: Manifest](v: T): T

  def op1_rep[T: Manifest](v: Rep[T])(implicit sc: SourceContext): Rep[T]

  // infix binary method
  def infix_op2[T: Manifest](a: String, i: T): String

  def infix_op2_rep[T: Manifest](a: Rep[String], i: Rep[T])(implicit sc: SourceContext): Rep[String]
  
  // TODO add other examples
  //  * Scala type classes
  //  * Interface from the OptiML

}

/**
* Simple DSL execution
*/
trait SimpleDSLExp extends BaseExp {

  def op1[T: Manifest](v: T): T = {
    println("op1")
    v
  }

  def op1_rep[T: Manifest](v: Rep[T])(implicit sc: SourceContext): Rep[T] = {
    System.out.println("REP op1")
    v
  }

  def infix_op2[T: Manifest](a: String, i: T): String = {
    System.out.println("infix_op2")
    a
  }

  def infix_op2_rep[T: Manifest](a: Rep[String], i: Rep[T])(implicit sc: SourceContext): Rep[String] = {
    System.out.println("NO REP infix_op2")
    a
  }
}

trait SimpleDSLGen extends ScalaGenBase {
  val IR: SimpleDSLExp
} 


/**
 * The DSL it self.
 */
trait MPDESimple extends SimpleDSL with MPDE {
  
  def test(u: Rep[Unit]): Rep[Unit] = lift {
    val res = "Hello" op2 op1(1)
 
    ()
  }

}

/**
 * Test suite.
 */
class TestMPDE extends FileDiffSuite {

  val prefix = "test-out/epfl/test14-"

  def testMPDE1 = {
    withOutFile(prefix + "simple-dsl") {
      new MPDESimple with SimpleDSLExp with IfThenElseExp with OrderingOpsExp { self =>
        val codegen = new SimpleDSLGen with  ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self }
        codegen.emitSource(test, "simple-dsl", new PrintWriter(System.out))
      }
    }
    //assertFileEqualsCheck(prefix + "staging1")
  }

}
