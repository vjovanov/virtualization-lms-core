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

    def op1_rep[T: Manifest](v: Rep[T]): Rep[T]= ???
  }

  // simple method
  def op1[T](v: T): T

  def op1_rep[T](v: Rep[T]): Rep[T]

  // infix binary method
  def infix_op2[T](a: String, i: T): String

  def infix_op2_rep[T](a: Rep[String], i: Rep[T]): Rep[String]
  
  // TODO add other examples
  //  * Scala type classes
  //  * Interface from the OptiML
  
}

/**
* Simple DSL execution
*/
trait SimpleDSLExp extends BaseExp {
  
  def op1[T](v: T): T = {
    println("op1")
    v
  }

  def op1_rep[T](v: Rep[T]): Rep[T] = {
    System.out.println("REP op1")
    v
  }

  def println(v: Any) = System.out.println(v)
  
  def println_rep(v: Rep[Any]) = System.out.println("rep:" + v)
  
  def infix_op2[T](a: String, i: T): String = {
    System.out.println("infix_op2")
    a
  }

  def infix_op2_rep[T](a: Rep[String], i: Rep[T]): Rep[String] = {
    System.out.println("REP infix_op2")
    a
  }
}

trait SimpleDSLGen extends ScalaGenBase {
  val IR: SimpleDSLExp
} 


/**
 * The simple DSL program.
 */
trait MPDESimple extends SimpleDSL with MPDE {
  // TODO temporary change for the macro should do the work of lifting by either:  
  // 1) finding corresponding conversion method for the type (by some convention)
  // 2) having a single lift method that magically lifts all the types. Magically means using type signature evidence. 
  implicit def intToRep(i: Int): Rep[Int] = unit(i)
  implicit def stringToRep(s: String): Rep[String] = unit(s)

  def test(u: Rep[Unit]): Rep[Unit] = lift {
    val x = "Hello" op2 op1(1)
    val y = op1(x)
    println(x)
    ()
  }: Unit // this annotations is here just for the moment since the desired interface is with reps

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
