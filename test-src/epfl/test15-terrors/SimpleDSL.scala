package scala.virtualization.lms
package epfl
package test15

import common._
import test1._

import scala.reflect.macros.Context
import language.experimental.macros
import util.OverloadHack
import java.io.{ PrintWriter, StringWriter, FileOutputStream }
import scala.reflect.SourceContext    

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
    println("REP op1")
    v
  }

  
  def infix_op2[T](a: String, i: T): String = {
    System.out.println("infix_op2")
    a
  }

  def infix_op2_rep[T](a: Rep[String], i: Rep[T]): Rep[String] = {
    System.out.println("NO REP infix_op2")
    a
  }
}

trait SimpleDSLGen extends ScalaGenBase {
  val IR: SimpleDSLExp
} 
