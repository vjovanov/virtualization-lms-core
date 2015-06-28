package scala.virtualization.lms
package epfl
package test15

import common._
import internal._
import test1._
import test7._
import test8.{ArrayMutation,ArrayMutationExp,ScalaGenArrayMutation}

import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext


import java.io._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader

trait Compile0 extends Base {

  def compile[B](f: ()=> Rep[B])(mB: Manifest[B]): ()=>B

}


trait DynCompile extends Expressions with DynamicBase {

  val codegen: ScalaCodegen { val IR: DynCompile.this.type }

  var compiler: Global = _
  var reporter: ConsoleReporter = _
  //var output: ByteArrayOutputStream = _

  def setupCompiler() = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
    */
    val settings = new Settings()

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    //settings.verbose.value = true
    // -usejavacp needed on windows?

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }

  var compileCount = 0

  var dumpGeneratedCode = false

  def compile[B](f: () => Exp[B])(mB: Manifest[B]): ()=>B = {
    if (this.compiler eq null)
      setupCompiler()

    val className = "staged$" + compileCount
    compileCount += 1

    val source = new StringWriter()
    val staticData = codegen.emitSource(Nil, codegen.reifyBlock(f())(mB), className, new PrintWriter(source))(mB)
    codegen.emitDataStructures(new PrintWriter(source))

    if (dumpGeneratedCode) println(source)

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
  //      compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")

    reporter.reset
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)

    val obj: ()=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[()=>B]
    obj
  }
}


trait DynCompileScala extends Compile0 with BaseExp with DynCompile

class Matrix(val m: Double, val n: Double, data: Array[Array[Double]]) {
  def *(that: Matrix) = {
    ??? // do multiplication
  }
}

class TestDynamicCompilation extends FileDiffSuite {

  // boilerplate definitions for DSL interface

  trait DSL extends LiftNumeric with NumericOps with PrimitiveOps with ArrayOps with BooleanOps
    with LiftVariables with DynIfThenElse with Print with DynamicBase with DynMatrixOps with OrderingOps {
    def staticData[T:Manifest](x: T): Rep[T]
    def test(): Rep[test15.Matrix]
  }

  trait Impl extends DSL with Runner with ArrayOpsExpOpt with NumericOpsExpOpt with PrimitiveOpsExp with OrderingOpsExpOpt with BooleanOpsExp
      with EqualExpOpt with VariablesExpOpt with StaticDataExp with BooleanOpsExpOpt
      with IfThenElseExpOpt with PrintExp with DynamicExp with DynMatrixOpsExp
      with DynCompile { self =>
    //override val verbosity = 1
    val codegen = new ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenStaticData with ScalaGenOrderingOps with ScalaGenArrayOps
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenBooleanOps with GenDynMatrixOps
      with ScalaGenPrint with DynamicGen /*with LivenessOpt*/ { val IR: self.type = self }

    val then = test()
    val guard = decisions.foldLeft(dunit(true): Exp[Boolean])((guard, dec) => boolean_and(guard, dec))
    println("Guards:")
    println(guard)
    // TODO put a transformer
    val res = if(guard) then else then
    codegen.emitSource(Nil, codegen.reifyBlock(res)(manifest[test15.Matrix]), "Test", new PrintWriter(System.out))(manifest[test15.Matrix])
    println("Decisions:")
    decisions.foreach(println)
    run()
  }

  /*
   * This is introduced as primitives can not be caught by the dynamic macro.
   */
  trait DynArith extends DynamicBase with PrimitiveOps with BooleanOps with DynIfThenElse with Equal with OrderingOps {
    def infix_+(lhs: Dyn[Int], rhs: Dyn[Int]): Dyn[Int] = {
      Both(
        lhs.static + rhs.static,
        lhs.dynamic + rhs.dynamic
      )
      }

    def infix_*(lhs: Dyn[Int], rhs: Dyn[Int]): Dyn[Int] =
      Both(lhs.static * rhs.static, lhs.dynamic * rhs.dynamic)

    def __equal(lhs: Dyn[Any], rhs: Dyn[Any]): Dyn[Boolean] =
      Both(lhs.static == rhs.static, lhs.dynamic == rhs.dynamic)

    def infix_unary_!(x: Dyn[Boolean]): Dyn[Boolean] =
      Both(!x.static, !x.dynamic)

    def infix_<(lhs: Dyn[Int], rhs: Dyn[Int]): Dyn[Boolean] =
      Both(lhs.static < rhs.static, lhs.dynamic < rhs.dynamic)

    def infix_&&(lhs: Dyn[Boolean], rhs: Dyn[Boolean]): Dyn[Boolean] =
      Both(lhs.static && rhs.static, lhs.dynamic && rhs.dynamic)

    def dassert(c: => Dyn[Boolean]): Rep[Unit] = if (infix_unary_!(c)) ???
  }

  //   trait MatrixOps { self: DSL =>

  //     implicit class MatrixOps(self: Rep[test15.Matrix]) {
  //       def m: Rep[Int] = _m(self)
  //       def n: Rep[Int] = _m(self)

  //       def *(that: Rep[test15.Matrix]): Rep[test15.Matrix] = mat_times(self, that)
  //       def +(that: Matrix): Matrix = mat_add(self, that)
  //     }

  //   case class NewMatrix(m: Rep[Int], n: Rep[Int], data: Rep[Array[Array[Double]]], cost: Rep[Int] = unit(0)) extends Rep[test15.Matrix]

  //   case class MatrixMult(lhs: Matrix, rhs: Matrix) extends Matrix {
  //     def m: Dyn[Int] = rhs.m
  //     def n: Dyn[Int] = lhs.n
  //     def cost: Dyn[Int] = lhs.cost + rhs.cost + lhs.n * rhs.n * rhs.m
  //   }

  //   case class MatrixPlus(lhs: Matrix, rhs: Matrix) extends Matrix {
  //     def m: Dyn[Int] = lhs.m
  //     def n: Dyn[Int] = rhs.n
  //     def cost: Dyn[Int] = lhs.cost + rhs.cost + m * n
  //   }
  //   // TODO need one reduction here
  // }

  trait DynMatrixOps extends DynArith { self: DSL =>
    def toRep(m: Matrix): Rep[test15.Matrix]
    trait Matrix {
      def m: Dyn[Int]
      def n: Dyn[Int]
      def cost: Dyn[Int]

      def *(that: Matrix): Matrix = {
        // dassert(this.n == that.m)
        MatrixMult(this, that)
      }
      def +(that: Matrix): Matrix = {
        // dassert(this.m == that.m && this.n == that.n)
        MatrixPlus(this, that)
      }
    }

    case class NewMatrix(
      m: Dyn[Int],
      n: Dyn[Int],
      data: Rep[Array[Array[Double]]],
      cost: Dyn[Int] = lift(0)) extends Matrix

    case class MatrixMult(lhs: Matrix, rhs: Matrix) extends Matrix {
      def m: Dyn[Int] = rhs.m
      def n: Dyn[Int] = lhs.n
      def cost: Dyn[Int] = lhs.cost + rhs.cost + lhs.n * rhs.n * rhs.m
    }

    case class MatrixPlus(lhs: Matrix, rhs: Matrix) extends Matrix {
      def m: Dyn[Int] = lhs.m
      def n: Dyn[Int] = rhs.n
      def cost: Dyn[Int] = lhs.cost + rhs.cost + m * n
    }
    // TODO need one reduction here
  }

  trait DynMatrixOpsExp extends DynMatrixOps { self: Impl =>
    case class MatrixCarrier(m: Matrix) extends Def[test15.Matrix]
    def toRep(m: Matrix): Rep[test15.Matrix] = MatrixCarrier(m)
  }

  trait GenDynMatrixOps extends GenericCodegen {
    val IR: DynMatrixOpsExp with Expressions with Impl
    import IR._

    class MatrixOrderOptimization(p: Array[Dyn[Int]]) {
      val n: Int = p.length - 1
      val m: scala.Array[scala.Array[Dyn[Int]]] = scala.Array.fill[Dyn[Int]](n, n)(lift(0))
      val s: scala.Array[scala.Array[Dyn[Int]]] = scala.Array.ofDim[Dyn[Int]](n, n)

      for (ii: Int <- 1 until n; i: Int <- 0 until n - ii) {
        val j: Int = i + ii
        m.apply(i).update(j, lift(scala.Int.MaxValue)) // TODO remove ArrayOps
        for (k <- i until j) {
          val q: Dyn[Int] = m(i)(k) + m(k + 1)(j) + p(i) * p(k + 1) * p(j + 1)
          if (q < m(i)(j)) {
            m(i)(j) = q
            s(i)(j) = lift(k)
          }
        }
      }

      def optimalChain(m: Array[IR.Matrix]): IR.Matrix = {
        def optimalChain0(i: Int, j: Int): IR.Matrix =
          if (i != j) optimalChain0(i, unlift(s(i)(j)))  *  optimalChain0(unlift((s(i)(j) + lift(1))), j)
          else m(i)

        optimalChain0(0, s.length-1)
      }
    }


    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case MatrixCarrier(matrices) => // generate multiplication
        def extractChain(m: IR.Matrix): List[NewMatrix] = m match {
          case x: NewMatrix => x :: Nil
          case MatrixMult(m1, m2) => extractChain(m1) ::: extractChain(m2)
        }
        val originalChain = extractChain(matrices)
        val sizes: List[Dyn[Int]] = originalChain.tail
          .foldLeft(scala.List(originalChain.head.m, originalChain.head.n))((agg, m) => agg :+ m.n)
        val optimizer = new MatrixOrderOptimization(sizes.toArray)
        val optimalChain =  optimizer.optimalChain(originalChain.toArray)

      case _ => super.emitNode(sym, rhs)
    }
  }

  // test case input data
  trait Runner extends Compile0 with DynamicBase {
    def test(): Rep[test15.Matrix]
    def run() {
      val f = compile(test)(manifest[Matrix])
      val v1 = f()
      println(v1)
    }
  }


  // staged program implementations
  val prefix = home + "test-out/epfl/test15-"

  def testDynamic = {
    withOutFileChecked(prefix+"dynamic-basic") {
      trait Prog extends DSL {
        def test(): Rep[test15.Matrix] = {
          val mat = Array(
             Array[Double](unit(1.0),unit(2),unit(3),unit(4),unit(5)),
             Array[Double](unit(1.0),unit(2),unit(3),unit(4),unit(5)),
             Array[Double](unit(1.0),unit(2),unit(3),unit(4),unit(5)),
             Array[Double](unit(1.0),unit(2),unit(3),unit(4),unit(5)),
             Array[Double](unit(1.0),unit(2),unit(3),unit(4),unit(5))
           )
          val x = NewMatrix(lift(2), lift(5), mat)
          val y = NewMatrix(lift(5), lift(2), mat)
          val z = NewMatrix(lift(2), lift(2), mat)
          toRep(x * y * z) // returning it back to LMS. TODO fix this it needs to be integrated. Implement a small code generator.
        }
      }
      new Prog with Impl
    }

  }
}
