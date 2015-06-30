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
import scala.collection.mutable

import java.io._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader

trait Compile0 extends BaseExp {

  def compile[B, Ret](syms: List[Sym[_]], f: => Rep[B])(mB: Manifest[B]): Ret

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

  def compile[B, Ret](syms: List[Sym[_]], f: => Exp[B])(mB: Manifest[B]): Ret = {
    if (this.compiler eq null)
      setupCompiler()

    val className = "staged$" + compileCount
    compileCount += 1

    val source = new StringWriter()
    val staticData = codegen.emitSource(syms, codegen.reifyBlock(f)(mB), className, new PrintWriter(source))(mB)
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

    val obj: Ret = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[Ret]
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
    with LiftVariables with DynIfThenElse with Print with DynamicBase with DynMatrixOps with OrderingOps with DynArith {
    def staticData[T:Manifest](x: T): Rep[T]
    def test(): Rep[Int]
  }

  trait Impl extends DSL with Runner with ArrayOpsExpOpt with NumericOpsExpOpt with PrimitiveOpsExp with OrderingOpsExpOpt with BooleanOpsExp
      with EqualExpOpt with VariablesExpOpt with StaticDataExp with BooleanOpsExpOpt
      with IfThenElseExpOpt with PrintExp with DynamicExp with DynMatrixOpsExp
      with DynCompile { self =>
    //override val verbosity = 1
    val UID: Long
    lazy val codegen = new ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenStaticData with ScalaGenOrderingOps with ScalaGenArrayOps
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenBooleanOps with GenDynMatrixOps
      with ScalaGenPrint with DynamicGen /*with LivenessOpt*/ { val IR: self.type = self }

    lazy val guardCodegen = new ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenStaticData with ScalaGenOrderingOps with ScalaGenArrayOps
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenBooleanOps with GenDynMatrixOps
      with ScalaGenPrint with DynamicGen /*with LivenessOpt*/ { val IR: self.type = self }

   def apply(v: Int): Int = {
    def constructGuards[T: Manifest]: Rep[T] = {
      def constructGuards0(current: Node, decisions: List[Boolean], taken: Boolean): Rep[T] = current match {
        case Leaf if taken => // call function
          emitLookup(orderedHoles, decisions.reverse)
        case Leaf if !taken => // recompile
          emitRecompile(orderedHoles, decisions.reverse)

        case DecisionNode(cond, tree, semanticPreserving, (left, takenLeft), (right, takenRight)) =>
          println("Taken: (" + takenLeft + ", " + takenRight + ")")
          val thn = constructGuards0(right, true :: decisions, takenRight)
          val els = constructGuards0(left, false :: decisions, takenLeft)

          if (tree) thn else els
      }

      constructGuards0(root, Nil, true)
    }

    def decisions[T: Manifest]: List[Boolean] = {
      def decisions0(current: Node, decisions: List[Boolean]): List[Boolean] = current match {
        case Leaf => decisions

        case DecisionNode(cond, tree, semanticPreserving, (left, _), (right, _)) =>
          if (cond) decisions0(right, cond :: decisions) else decisions0(left, !cond :: decisions)
      }

      decisions0(root, Nil)
    }


    if (CodeCache.guard.contains(UID)) {
      val (_, guard: (Int => Int), _) = CodeCache.guard(UID)
      guard(v)
    } else {
      // define recompile
      def recompile(hole: Int): Int => Int = {
        // reset() // TODO here the decisions need to be kept
        holes.clear()
        currentNode = None
        if (CodeCache.guard.contains(UID)) {
          val (_, _, decisions) = CodeCache.guard(UID)
          root = decisions
          println("Decisions: " + CodeCache.guard(UID)._3)
          currentNode = None
        }
        val code = test()
        val guards = constructGuards[Int]
        val decs = decisions[Int].reverse
        // TODO for testing purposes
        println("Guard:")
        guardCodegen.emitSource(orderedHoles, codegen.reifyBlock(guards)(manifest[Int]), "Test", new PrintWriter(System.out))(manifest[Int])
        println("Code:")
        codegen.emitSource(orderedHoles, codegen.reifyBlock(code)(manifest[Int]), "Test", new PrintWriter(System.out))(manifest[Int])

        val guardFunction = compile[Int, Int => Int](orderedHoles, guards)(manifest[Int])
        val function = compile[Int, Int => Int](orderedHoles, code)(manifest[Int])

        CodeCache.code  += ((UID, decs)-> function)
        CodeCache.guard += (UID -> (recompile _, guardFunction, root))
        function
      }
      // TODO pass the values
      recompile(1)(1)
    }
    }

    override def reset() = {
      super.reset
      root = Leaf // TODO this needs to be kept
      holes.clear()
      currentNode = None
    }
  }

  /*
   * This is introduced as primitives can not be caught by the dynamic macro.
   */
  trait DynArith extends DynamicBase with PrimitiveOps with BooleanOps with DynIfThenElse with Equal with OrderingOps {
    def infix_+(lhs: Dyn[Int], rhs: Dyn[Int]): Dyn[Int] =
      Both(lhs.static + rhs.static, lhs.dynamic + rhs.dynamic)

    def infix_*(lhs: Dyn[Int], rhs: Dyn[Int]): Dyn[Int] =
      Both(lhs.static * rhs.static, lhs.dynamic * rhs.dynamic)

    def __equal(lhs: Dyn[Any], rhs: Dyn[Any]): Dyn[Boolean] =
      Both(lhs.static == rhs.static, lhs.dynamic == rhs.dynamic)

    def infix_unary_!(x: Dyn[Boolean]): Dyn[Boolean] =
      Both(!x.static, !x.dynamic)

    def infix_<(lhs: Dyn[Int], rhs: Dyn[Int]): Dyn[Boolean] =
      Both(lhs.static < rhs.static, lhs.dynamic < rhs.dynamic)

    def infix_>(lhs: Dyn[Int], rhs: Dyn[Int]): Dyn[Boolean] =
      Both(lhs.static > rhs.static, lhs.dynamic > rhs.dynamic)

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
    def test(): Rep[Int]
  }


  // staged program implementations
  val prefix = home + "test-out/epfl/test15-"

  def testBasicIf = {
    var x = 1
    withOutFileChecked(prefix+"dynamic-basic") {
      trait Prog extends DSL {
        def test(): Rep[Int] = {
          if (hole[Int](x, 1) < lift(20))
            if (hole[Int](x, 1) < lift(10)) 0 else 1
          else 2
        }
      }

      println((new {val UID = 5551L} with Prog with Impl)(x))
      x = 11
      println((new {val UID = 5551L} with Prog with Impl)(x))
      x = 20
      println((new {val UID = 5551L} with Prog with Impl)(x))
      x = 1
      println((new {val UID = 5551L} with Prog with Impl)(x))
      x = 11
      println((new {val UID = 5551L} with Prog with Impl)(x))
      // x = 20
      // println((new {val UID = 5551L} with Prog with Impl)(x))
    }

  }

  /*def testDynamic = {
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

  }*/
}
