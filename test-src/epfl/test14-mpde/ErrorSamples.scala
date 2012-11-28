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
      print(a)
      print(b)
      print(c)
      print(d)
    }

    {
      val i = 1
      val s = "y"
      val b = true
      val d = 0.1

      // this works because the implicit gets invoked on the method      
      testMethod(i, s, b, d)
      // Users tend to write the following (see project stanford-ppl/Delite with grep -rn "unit(" .) 
      //      val i = unit(1)
      //      val s = unit("y")
      //      val b = unit(true)
      //      val d = unit(0.1)
      // TODO investigate why is that? Try to remove them in Delite and see what fails. 
    }

    // assignment to variables does not work properly
    // we need to fix the scala-virtualized first and then reason about issues.
    // FIXME one thing that will certainly fail is when we initialize variable with a constant.
    {  // all var problems can be fixed by removing the bug in EmbeddedControls
      
      /* FIXME This throws an exception at compilation time. Scala virtualized-bug.
       var scala = 2
       scala = 2.10
       scala = "perl"
       */

      // FIXME this throws a class cast exception
      try {
        var scala = 2
        scala = "perl"
        print(scala)
      } catch {
        case e: Throwable => ()
      }

      // FIXME this throws a class cast exception and should fail at compile time.
      try {
        var scala = unit(2)
        scala = unit("perl")
      } catch {
        case e: Throwable => System.out.println("")
      }

      // FIXME this should work
      {
        var i: Rep[Int] = 1
        var b: Rep[Boolean] = true
        var d: Rep[Double] = 0.1
        // FIXME throws out of permgen space
        //var s: Rep[String] = unit("y")
        //s = "k"
        var s: Rep[String] = unit("y")

        testMethod(i, s, b, d)
      }
    }

    // TODO see what happens with the code below. How does var definition work in nested scopes. 
    /* Why does this fail? TODO Investigate and try to fix if SourceContexts are broken. */
    {
      var scala = unit("scala 2")
      // FIXME if we remove the line below the compiler thinks that we are providing the manifest.
      // Since the macro will also take care of the manifests we should not have this issue.  
      
      {
          var scala = unit("dotty")
          
          print(scala)          
      }
    }

    ()
  }

  // TODO algebraic data types
  
  /**
   * This test shows how regular scala constructs can be mixed with DSL code. This can create confusion with non-proficient users.
   */
  def whitelistOperations(v: Rep[Unit]): Rep[Unit] = {
    // there should be a whitelist of language features
    // and a nice warning that notes that they are banned
    {
      // FIXME The following operations should not be allowd in the DSL code       
      val y = scala.collection.immutable.List(1 -> 3) match {
        case scala.collection.immutable.List((1, 3)) => "Nice DSL feature that does not exist"
      }

      // This should not be allowed in the DSL
      val z = try {
        throw new RuntimeException("?????")
      } catch { case e: RuntimeException => () }

      // This should be lifted
      println("hey")
    }
  }

  /**
   * The test that shows errors related to tuples.
   *
   * If we leave explicit control about the Rep types to the user we get a complex language that is hard to use.
   * If the macro searches for the best match we can get into a very complex macro, or even exponential search space explosion.
   * Check this on an example!!!
   */
  def tuples(v: Rep[Unit]): Rep[Unit] = {

    def outer(t: Rep[(Int, Int)]): Unit = print(t)
    def inner(t: Tuple2[Rep[Int], Rep[Int]]): Unit = print(t)

    val x = (1, 2)
    // outer(x) // does not compile
    // outer((1,2)) // does not compile
    // TODO can this be fixed by better implicits

    val xa: (Rep[Int], Rep[Int]) = (1, 2)
    outer(xa)

    val xu = unit((1, 2))
    outer(xu)

    {
      val v = unit(2)

      // FIXME it should work without the unit
      val z = (unit(1), v)
      inner(z)
    }

    inner((1, 2))

  }

  def intop(x: Rep[Int]) = print(x)
  def intmap(x: Rep[Int] => Rep[Int]) = x(1)

  // TODO Type inference issues 
  // closure returns a constant: {x => 1} will be inferred as Rep[?] => Int.
  // closure returns an conditional: if (passes > max_passes) unit(0) else max_passes - passes  
  /**
   * This test finds the type inference related errors.
   */
  def inference(u: Rep[Unit]): Rep[Unit] = {
    def m(x: Rep[Int]) = print(x.toString)

    val number = unit(1)
    val cond = unit(true)

    // What will be the type of x? How is this achieved? 
    {
      val x = if (cond) number else 1
      intop(x)
    }

    // FIXME
    // m(x) // out of PermGen space TODO investigate

    {
      // how does this work? 
      val x = if (unit(true)) if (unit(true)) { print(""); 1 } else { print(""); 2 } else 4
      intop(x)
    }

    { // how does this work?
      val v = unit(1)
      intmap(x => if (unit(true)) 0 else v)
    }

    // FIXME methods does not have unit included if it is of type Rep[Unit]
    {
      // def m: Rep[Unit] = 1
    }
    ()
  }

  /**
   * This test shows how recursion must specially annotated. The macro based approach should allow recursion as in regular
   * Scala.
   */
  def recursion(u: Rep[Unit]): Rep[Unit] = {

    def gcd(x: Rep[Int], y: Rep[Int]): Rep[Int] =
      if (y == 0) x
      else gcd(x, x % y)

    // FIXME this causes the stack overflow.
    // gcd(1213, 898987)

    ()
  }

  /**
   * This test shows examples of ugly error messages. This can be added later as we do the work.
   * I have seen a lot of them so I know they exist.
   */
  def typeErrors(u: Rep[Unit]): Rep[Unit] = {

  }

}

class TestRepTypesErrors extends FileDiffSuite {

  val prefix = "test-out/epfl/test14-"

  def testPolymorphicStaging1 = {
    withOutFile(prefix + "var-val") {
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

  def testPolymorphicStaging3 = {
    withOutFile(prefix + "recursion") {
      new RepDSL with ScalaOpsPkgExp { self =>
        val codegen = new ScalaCodeGenPkg with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self }
        codegen.emitSource(recursion, "recursion", new PrintWriter(System.out))
      }
    }
    //assertFileEqualsCheck(prefix + "staging1")
  }

  def testPolymorphicStaging4 = {
    withOutFile(prefix + "whitelistOperations") {
      new RepDSL with ScalaOpsPkgExp { self =>
        val codegen = new ScalaCodeGenPkg with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self }
        codegen.emitSource(whitelistOperations, "whitelistOperations", new PrintWriter(System.out))
      }
    }
    //assertFileEqualsCheck(prefix + "staging1")
  }

  def testPolymorphicStaging5 = {
    withOutFile(prefix + "tuples") {
      new RepDSL with ScalaOpsPkgExp { self =>
        val codegen = new ScalaCodeGenPkg with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self }
        codegen.emitSource(tuples, "tuples", new PrintWriter(System.out))
      }
    }
    //assertFileEqualsCheck(prefix + "staging1")
  }

}
