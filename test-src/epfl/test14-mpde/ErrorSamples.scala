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
 * Test that shows type system problems with Rep[T] DSL Interface. The test should flush out what is not good with Rep based approach:
 *   1) Type errors are not readable. There are many wierd errors that the developer can not reason about
 *   2) Requires invocation of the unit parameter in some occasions
 *      * Variable assignment with a constant
 *      * Tuples
 *   3) Does not allow recursion without annotations
 *   4) Does not prevent the user from using scala language features
 *   5) Pollutes the method signatures
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

    // this works because the implicit gets invoked on the method
    {
      val i = 1
      val s = "y"
      val b = true
      val d = 0.1

      testMethod(i, s, b, d)
    }

    // assignment to variables does not work properly
    {
      /*
       * This throws an exception at compilation time. Scala virtualized-bug.
       * var scala = 2
       * scala = 2.10
       * scala = "perl"
       */

      // this throws a class cast exception
      try {
        var scala = 2
        scala = "perl"
        print(scala)
      } catch {
        case e: Throwable => ()
      }

      // this throws a class cast exception
      try {
        var scala = unit(2)
        scala = unit("perl")
      } catch {
        case e: Throwable => System.out.println("")
      }

      // there should be a white list of language features
      // and a nice warning that bans them if they are not allowed
      {
        // who allows the following
        val x = if (true) 1 else 2

        val y = scala.collection.immutable.List(1 -> 3) match { case scala.collection.immutable.List((1, 3)) => "nice DSL feature that does not exist" }

        // what does this code do
        val z = try {
          throw new RuntimeException("?????")
        } catch { case e: RuntimeException => () }

        println(x)
      }

      /* Why does this fail?    
    {
        var scala = unit("scala 1")
        {
          var scala = unit("scala 2")
          {
            var scala = unit("dotty")
            System.out.println(scala)
          }
          System.out.println(scala)
        }
        System.out.println(scala)
      }
    }*/

      {
        var scala = unit("scala 1")

        {
          var scala = unit("scala 2")

          {
            var scala = unit("dotty")

            print(scala)
          }

          print(scala)

        }

        print(scala)
      }

      {
        var scala = "scala 1"

        {
          var scala = "scala 2"

          {
            var scala = "dotty"

            print(scala)
          }

          print(scala)

        }

        print(scala)
      }

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

  /**
   * How to reason about this???
   *
   * If we leave explicit control about the Rep types to the user we get a complex language that is hard to use.
   * If the macro searches for the best match we can get into a very complex macro, or even exponential code explosion. Check this on an example!!!
   */
  def tuples: Rep[Unit] = {
    
    def outer(t: Rep[(Int, Int)]): Unit = () 
    
    def inner(t: Tuple2[Rep[Int], Rep[Int]]): Unit = ()

    val x = (1, 2)
    // outer(x) // does not compile
    
    // outer((1,2)) // does not compile
    
    val xa: (Rep[Int], Rep[Int]) = (1, 2)
    outer(xa)
    
    val xu = unit((1, 2))
    print(outer(xu))

    val y = unit((1, 2))
    outer(y)
    
    val v = unit(2)
    
    val z = (unit(1), v)
    inner(z)

    
    inner((1, 2))
    
    // how would it go for nested rep types???
    /*
     * This is not and should not be possible. Rep[Rep] causes a missing Manifest which makes sense. 
     * def allrep(t: Rep[(Rep[Int], Rep[Int], Rep[Int])]) = t
     * val all = unit((unit(1), unit(2), unit(3)));
     * allrep(all)
     */
        
  }
  
  
  /**
   * This test finds the type inference related errors.
   */
  def inference(u: Rep[Unit]): Rep[Unit] = {
     def m(x: Rep[Int]) = print(x) 
     
     val number = unit(1)
     val cond = unit(true)
     
     
     val x = if (cond) 1 else number
     
     System.out.println(x.type)
     // m(x) // out of PermGen space
     
     
  }
 
  /**
   * This test shows how recursion must specially annotated. The macro based approach should allow recursion. 
   */
  def recursion(u: Rep[Unit]): Rep[Unit] = {
            
  }
  
}

class TestRepTypesErrors extends FileDiffSuite {

  val prefix = "test-out/epfl/test14-"

  def testPolymorphicStaging1 = {
    withOutFile(prefix + "type-errors") {
      new RepDSL with ScalaOpsPkgExp { self =>
        val codegen = new ScalaCodeGenPkg with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self }
        codegen.emitSource(valVarDef, "valVarDef", new PrintWriter(System.out))
      }
    }
    //assertFileEqualsCheck(prefix + "staging1")
  }
  
  def testPolymorphicStaging2 = {
    withOutFile(prefix + "inference") {
      new RepDSL with ScalaOpsPkgExp { self =>
        val codegen = new ScalaCodeGenPkg with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self }
        codegen.emitSource(inference, "inference", new PrintWriter(System.out))
      }
    }
    //assertFileEqualsCheck(prefix + "staging1")
  }

}
