package scala.virtualization.lms
package epfl
package test8

import common._
import test1._

import test7.{Print,PrintExp,ScalaGenPrint}
import internal.ScalaCompile

import scala.util.continuations._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

trait CpsProg extends Arith with IfThenElse with Equal with Print with Compile {
  
  def choose[A:Manifest](x: Rep[Boolean]): Boolean @cps[Rep[A]] = shift { k: (Boolean => Rep[A]) =>
    if (x)
      k(true)
    else
      k(false)
  }
  
  def test(x: Rep[Boolean]) = { // recompile
    reset {
      //val z = choose(x)
      shift { k:(Unit=>Unit) => k(()) }
    }
    this.print(x)
  }
  
}


class TestCPS extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test8-"
  
  def testCps1 = {
    withOutFile(prefix+"cps1") {
      new CpsProg with ArithExp with EqualExp with IfThenElseExp with PrintExp with ScalaCompile { self =>
        val codegen = new ScalaGenArith with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        //override def compile
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"cps1")
  }
 
}