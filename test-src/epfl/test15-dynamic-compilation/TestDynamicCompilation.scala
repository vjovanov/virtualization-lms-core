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
import breeze.linalg._
import scala.tools.nsc.interpreter.AbstractFileClassLoader

trait Compile0 extends BaseExp {

  def compile[R](syms: List[Sym[_]], f: => Rep[R])(mR: Manifest[R]): Any

}


trait DynCompile extends Expressions with DynamicBase with Blocks {

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

  def compile[B](syms: List[Sym[_]], f: => Exp[B])(mB: Manifest[B]): Any =
    compileBlock[B](syms, codegen.reifyBlock(f)(mB))(mB)

  def compileBlock[B](syms: List[Sym[_]], f: codegen.Block[B])(mB: Manifest[B]): Any = {
    if (this.compiler eq null)
      setupCompiler()

    val className = "staged$" + compileCount
    compileCount += 1

    val source = new StringWriter()
    val staticData = codegen.emitSource(syms, f, className, new PrintWriter(source))(mB)
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

    cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*)
  }
}


trait DynCompileScala extends Compile0 with BaseExp with DynCompile


class TestDynamicCompilation extends FileDiffSuite {

  // boilerplate definitions for DSL interface

  trait MatrixChainTransformer extends ForwardTransformer {
    val IR: ImplLike
    import IR._

    class MatrixOrderOptimization(p: Array[Dyn[Int]]) {
      println("Matrices: " + p.map(_.static).toList.mkString("[", ",", "]"))
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

      def optimalChain(m: Array[DMatrix]): Exp[DenseMatrix[Double]] = {
        def optimalChain0(i: Int, j: Int): Exp[DenseMatrix[Double]] =
          if (i != j) MatrixMult(optimalChain0(i, unlift(s(i)(j))), optimalChain0(unlift((s(i)(j) + lift(1))), j))
          else m(i)

        optimalChain0(0, s.length-1)
      }
    }

    val chains: mutable.Map[Sym[_], List[DMatrix]] = mutable.Map.empty[Sym[_], List[DMatrix]]

    override def transformStm(stm: Stm): Exp[Any] = stm match {

      case TP(s, m@DMatrix(l, r, Def(Hole(nr)))) =>
        chains.update(s, List(m))
        println(m)
        super.transformStm(stm)

      case TP(s, mult@DMatrix(_, _, Def(MatrixMult(l: Sym[_], r: Sym[_])))) =>
        chains.update(s, chains(l) ++ chains(r))
        println(chains(l).map(x => (x.m.static, x.n.static, x)).mkString("", " X ", ""))
        println(chains(r).map(x => (x.m.static, x.n.static)).mkString("", " X ", ""))
        super.transformStm(stm)

      case c@TP(s, Chain(x: Sym[_])) =>
        // do the reordering
        val originalChain = chains(x)
        val sizes: List[Dyn[Int]] = originalChain.tail
          .foldLeft(scala.List(originalChain.head.m, originalChain.head.n))((agg, m) => agg :+ m.n)

        println("Original chain: ")
        println(originalChain.map(x => (x.m.static, x.n.static) + " X "))
        println("Sizes: ")
        println(sizes)
        val optimizer = new MatrixOrderOptimization(sizes.toArray)
        optimizer.optimalChain(originalChain.toArray)

      case _ =>
        super.transformStm(stm)
    }
  }

  trait DSL extends LiftNumeric with common.NumericOps with PrimitiveOps with ArrayOps with BooleanOps
    with LiftVariables with DynIfThenElse with Print with DynamicBase with OrderingOps with DynArith with MatrixOps with DynMatrixOps with OverloadHack {
    def staticData[T:Manifest](x: T): Rep[T]
  }
  trait ImplLike extends DSL with ArrayOpsExpOpt with NumericOpsExpOpt with PrimitiveOpsExp with OrderingOpsExpOpt with BooleanOpsExp
      with EqualExpOpt with VariablesExpOpt with StaticDataExp with BooleanOpsExpOpt
      with IfThenElseExpOpt with PrintExp with DynamicExp with MatrixOpsExp with Transforming
      with DynCompile with DynMatrixOpsExp
  abstract class Impl[Ret: Manifest] extends ImplLike { self =>
    //override val verbosity = 1
    val UID: Long
    lazy val codegen = new ScalaGenNumericOps with ScalaGenPrimitiveOps with ScalaGenStaticData with ScalaGenOrderingOps with ScalaGenArrayOps
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenBooleanOps
      with ScalaGenPrint with ScalaGenEqual with GenMatrixExp with DynamicGen with GenDynMatrixExp { val IR: self.type = self
         override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {

           case PEU(x) =>
             stream.println(s"println(${quote(x)})")
             emitValDef(sym, quote(x))

           case _ => super.emitNode(sym, rhs)
     }
      }


   // TODO Remove
   case class PEU(x: Exp[Boolean]) extends Def[Boolean]
   def printAndUse(x: Exp[Boolean]): Exp[Boolean] = PEU(x)

   private final def constructGuards[T: Manifest]: Rep[T] = {
      def constructGuards0(current: Node, decisions: List[Boolean]): Rep[T] = current match {
        case Leaf(Some(leafDecisions)) => // call function
          assert(leafDecisions == decisions)
          emitLookup[T](orderedHoles, decisions)
        case Leaf(None) => // recompile
          emitRecompile[T](orderedHoles, decisions)

        case DecisionNode(tree, semanticPreserving, left, right) =>
          val thn = constructGuards0(right, decisions :+ true)
          val els = constructGuards0(left, decisions :+ false)
          if (printAndUse(tree)) thn else els
      }

      constructGuards0(root, Nil)
   }
   private final def recompile(run: Any) = {
      globalDefs.foreach(println)
      holes.clear()
      decisions = Nil
      parent = None
      val matrixMultTransformer = new MatrixChainTransformer { val IR: self.type = self }
      if (CodeCache.guard.contains(UID)) {
        val (_, _, explored) = CodeCache.guard(UID)
        root = explored
        println("Root: " + root)
        parent = None
      }
      val code: Rep[Ret] = main()

      // semantic preserving
      val transformedCode = matrixMultTransformer(codegen.reifyBlock(code))

      val guards: Rep[Ret] = constructGuards[Ret]

      val decs = decisions
      println("decisions:" + decisions)
      // TODO for testing purposes
      println(orderedHoles)
      println("Guard:")
      codegen.emitSource(orderedHoles, codegen.reifyBlock(guards)(manifest[Ret]), "Guard", new PrintWriter(System.out))(manifest[Ret])
      println("Code:")
      codegen.emitSource(orderedHoles, transformedCode, "Code", new PrintWriter(System.out))(manifest[Ret])

      val guardFunction = compile[Ret](orderedHoles, guards)(manifest[Ret])
      val function = compileBlock[Ret](orderedHoles, transformedCode)(manifest[Ret])

      CodeCache.code.update((UID, decs), function)
      CodeCache.guard.update(UID, (run, guardFunction, root))
      function
    }

   def main(): Rep[Ret]

   def apply[T0: Manifest, T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest](
    v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9): Ret = {
     def getGuard[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, Ret]: (T0, T1, T2, T3, T4, T5, T6, T7, T8, T9) => Ret =
       CodeCache.guard(UID)._2.asInstanceOf[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9) => Ret]

     if (CodeCache.guard.contains(UID)) {
       getGuard[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, Ret](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9)
     } else { // on the first run
       def recompileRun(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9): Ret =
         recompile(recompileRun _).asInstanceOf[(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9) => Ret](v0, v1, v2, v3, v4, v5, v6, v7, v8, v9)

       recompileRun(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9)
     }
   }

   def apply[T0: Manifest, T1: Manifest, T2: Manifest](v0: T0, v1: T1, v2: T2): Ret = {
     def getGuard[T0, T1, T2, Ret]: (T0, T1, T2) => Ret =
       CodeCache.guard(UID)._2.asInstanceOf[(T0, T1, T2) => Ret]

     if (CodeCache.guard.contains(UID)) {
       getGuard[T0, T1, T2, Ret](v0, v1, v2)
     } else { // on the first run
       def recompileRun(v0: T0, v1: T1, v2: T2): Ret = recompile(recompileRun _).asInstanceOf[(T0, T1, T2) => Ret](v0, v1, v2)

       recompileRun(v0, v1, v2)
     }
   }

   def apply[T0: Manifest](v: T0):  Ret = {
     def getGuard[T0, Ret]: T0 => Ret =
       CodeCache.guard(UID)._2.asInstanceOf[T0 => Ret]

     if (CodeCache.guard.contains(UID)) {
       getGuard[T0, Ret](v)
     } else { // on the first run
       def recompileRun(v0: T0): Ret = recompile(recompileRun _).asInstanceOf[T0 => Ret](v0)

       recompileRun(v)
     }
   }

    override def reset() = {
      super.reset
      root = emptyRoot // TODO this needs to be kept
      holes.clear()
      parent = None
    }
  }

  /*
   * This is introduced as primitives can not be caught by the dynamic macro.
   */
  trait DynArith extends DynamicBase with PrimitiveOps with BooleanOps with DynIfThenElse with Equal with OrderingOps {
    def infix_+(lhs: Dyn[Int], rhs: Dyn[Int]): Dyn[Int] =
      Both(lhs.static + rhs.static, lhs.dynamic + rhs.dynamic)

    def infix_-(lhs: Dyn[Int], rhs: Dyn[Int]): Dyn[Int] =
      Both(lhs.static - rhs.static, lhs.dynamic - rhs.dynamic)

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

  trait MatrixOps { self: DSL =>

    implicit class MatrixOps(l: Rep[DenseMatrix[Double]]) {
      def m: Rep[Int] = matrix_m(l)
      def n: Rep[Int] = matrix_n(l)
      def *(r: Rep[DenseMatrix[Double]]): Rep[DenseMatrix[Double]] = matrix_*(l,r)
    }

    def matrix_*(l: Rep[DenseMatrix[Double]], r: Rep[DenseMatrix[Double]]): Rep[DenseMatrix[Double]]
    def matrix_m(l: Rep[DenseMatrix[Double]]): Rep[Int]
    def matrix_n(l: Rep[DenseMatrix[Double]]): Rep[Int]
  }

  trait MatrixOpsExp extends MatrixOps with Transforming { self: ImplLike =>
    case class MatrixMult(l: Rep[DenseMatrix[Double]], r: Rep[DenseMatrix[Double]]) extends Def[DenseMatrix[Double]]
    case class MatrixN(l: Rep[DenseMatrix[Double]]) extends Def[Int]
    case class MatrixM(l: Rep[DenseMatrix[Double]]) extends Def[Int]

    def matrix_*(l: Rep[DenseMatrix[Double]], r: Rep[DenseMatrix[Double]]): Rep[DenseMatrix[Double]] = MatrixMult(l, r)
    def matrix_m(l: Rep[DenseMatrix[Double]]): Rep[Int] = MatrixM(l)
    def matrix_n(l: Rep[DenseMatrix[Double]]): Rep[Int] = MatrixN(l)

    override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
       case MatrixMult(l, r) => matrix_*(f(l), f(r))
       case MatrixN(l) => matrix_n(f(l))
       case MatrixM(l) => matrix_m(f(l))
       case Hole(n) => toAtom(Hole(n))
       case _ => super.mirror(e,f)
     }).asInstanceOf[Exp[A]] // why??
  }

  trait GenMatrixExp extends ScalaGenBase {
     val IR: MatrixOpsExp with Expressions with ImplLike
     import IR._
     override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
       case MatrixMult(l, r) => emitValDef(sym, quote(l) + " * " + quote(r))
       case MatrixN(l) => emitValDef(sym, quote(l) + ".cols")
       case MatrixM(l) => emitValDef(sym, quote(l) + ".rows")
       case _ => super.emitNode(sym, rhs)
     }

  }

  trait DynMatrixOps extends MatrixOps { self: DSL =>
    type DynMatrix
    implicit class DynMatrixOps(l: DynMatrix) {
      def m: Dyn[Int] = matrix_m(l)
      def n: Dyn[Int] = matrix_n(l)
      def *(r: DynMatrix): DynMatrix = matrix_*(l,r)
    }

    def matrix_*(l: DynMatrix, r: DynMatrix)(implicit h:Overloaded1): DynMatrix
    def matrix_m(l: DynMatrix)(implicit h:Overloaded1): Dyn[Int]
    def matrix_n(l: DynMatrix)(implicit h:Overloaded1): Dyn[Int]
    def holeM(x: DenseMatrix[Double], nr: Int): DynMatrix
    def chain(x: DynMatrix): Rep[DenseMatrix[Double]]
  }

    trait DynMatrixOpsExp extends DynMatrixOps with Transforming { self: ImplLike =>
      type DynMatrix = Exp[DenseMatrix[Double]] with Static

      case class Chain(x: DynMatrix) extends Def[DenseMatrix[Double]]
      final case class DMatrix(m: Dyn[Int], n: Dyn[Int], matrix: Rep[DenseMatrix[Double]]) extends Def[DenseMatrix[Double]]
      implicit def toSDAtom(v: Def[DenseMatrix[Double]]): Exp[DenseMatrix[Double]] with Static = toAtom(v).asInstanceOf[Exp[DenseMatrix[Double]] with Static]

      def holeM(x: DenseMatrix[Double], nr: Int): DynMatrix ={
        val mat = holeRep[DenseMatrix[Double]](x, nr)
        println("Hole:" + DMatrix(Both(x.rows, mat.m), Both(x.cols, mat.n), mat))
        DMatrix(Both(x.rows, mat.m), Both(x.cols, mat.n), mat)
      }

      def chain(x: DynMatrix): Rep[DenseMatrix[Double]] = Chain(x)

      def matrix_*(l: DynMatrix, r: DynMatrix)(implicit h:Overloaded1): DynMatrix = {
        DMatrix(l.m, r.n, MatrixMult(l, r))
      }

      def matrix_m(l: DynMatrix)(implicit h:Overloaded1): Dyn[Int] = l match {
        case Def(DMatrix(l, _, _ )) => l
      }
      def matrix_n(l: DynMatrix)(implicit h:Overloaded1): Dyn[Int] = l match {
        case Def(DMatrix(_, r, _ )) => r
      }

      override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
       case DMatrix(l, r, matrix) => toSDAtom(DMatrix(Both(l.static, f(l.dynamic)), Both(r.static, f(r.dynamic)), f(matrix)))
       case _ => super.mirror(e,f)
     }).asInstanceOf[Exp[A]] // why??
    }

  trait GenDynMatrixExp extends ScalaGenBase {
     val IR: MatrixOpsExp with Expressions with ImplLike
     import IR._
     override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
       case DMatrix(cols, rows, m) => emitValDef(sym, quote(m))
       case _ => super.emitNode(sym, rhs)
     }
  }


  // staged program implementations
  val prefix = home + "test-out/epfl/test15-"

  def testBasicIf = {
    var x = 1
    withOutFileChecked(prefix+"dynamic-basic") {
      class Prog(val UID: Long = 5551L) extends Impl[Int] {
        def main(): Rep[Int] = {
          if (hole[Int](x, 1) < lift(20))
            if (hole[Int](x, 1) < lift(10)) 0 else 1
          else 2
        }
      }

      println((new Prog)(x))
      x = 11
      println((new Prog)(x))
      x = 20
      println((new Prog)(x))
      x = 1
      println((new Prog)(x))
      x = 11
      println((new Prog)(x))
      x = 20
      println((new Prog)(x))
    }

  }

  def testpow = {
    var x = 1
    withOutFileChecked(prefix+"dynamic-pow") {
      class Prog(val UID: Long = 5552L) extends Impl[Int] {
        def main(): Rep[Int] = {
          def pow(base: Rep[Int], exp: Dyn[Int]): Rep[Int] = {
            if (exp == lift(0)) 1
            else base * pow(base, exp - lift(1))
          }

          pow(unit(2), hole[Int](x,1))
        }
      }
      println((new Prog)(x))
      x = 1
      println((new Prog)(x))
      x = 5
      println((new Prog)(x))
      x = 4
      println((new Prog)(x))
      x = 3
      println((new Prog)(x))
      x = 5
      println((new Prog)(x))
      x = 5
      println((new Prog)(x))
      x = 1
      println((new Prog)(x))
    }

  }

  def test3inputs = {
    var (x, y, z) = (1, 2, 3)
    withOutFileChecked(prefix+"dynamic-3inputs") {
      class Prog(val UID: Long = 5552L) extends Impl[Int] {
        def main(): Rep[Int] = {
          if (hole[Int](x,1) > hole[Int](y,2) + hole[Int](z, 3)) unit(1) else unit(2)
        }
      }
      println((new Prog)(x))
      x = 1
      println((new Prog)(x))
      x = 5
      println((new Prog)(x))
      x = 4
      println((new Prog)(x))
      x = 3
      println((new Prog)(x))
      x = 5
      println((new Prog)(x))
      x = 5
      println((new Prog)(x))
      x = 1
      println((new Prog)(x))
    }

  }

  def testMatrixMult = {

    def mat(cols: Int, rows: Int) = DenseMatrix.zeros[Double](cols, rows)
    var x = mat(2, 22)
    var y = mat(22, 2)
    var z = mat(2, 2)




    /*withOutFileChecked(prefix+"basic-matrices") {
      class Prog(val UID: Long = 5553L) extends Impl[DenseMatrix[Double]] {
        def main(): Rep[DenseMatrix[Double]] = {
          val m2 = holeM(x, 1) * holeM(y, 2)
          chain(m2 * holeM(z, 3))
        }
      }
      println(s"[${x.cols}, ${x.rows}, ${y.rows}, ${z.rows}]")
      println((new Prog)(x, y, z))

      x = mat(3, 22)
      y = mat(22, 2)
      z = mat(2, 2)

      println(s"[${x.cols}, ${x.rows}, ${y.rows}, ${z.rows}]")
      println((new Prog)(x, y, z))
      x = mat(100, 30)
      y = mat(30, 500)
      z = mat(500, 6000)
      println(s"[${x.cols}, ${x.rows}, ${y.rows}, ${z.rows}]")
      println((new Prog)(x, y, z))
      println(s"[${x.cols}, ${x.rows}, ${y.rows}, ${z.rows}]")
      println((new Prog)(x, y, z))

      x = mat(3, 22)
      y = mat(22, 2)
      z = mat(2, 2)

      println(s"[${x.cols}, ${x.rows}, ${y.rows}, ${z.rows}]")
      println((new Prog)(x, y, z))
    }*/


    var (v0, v1, v2, v3, v4, v5, v6, v7, v8, v9) = (mat(2,2),mat(2,2),mat(2,2),mat(2,2),mat(2,2),mat(2,2),mat(2,2),mat(2,2),mat(2,2),mat(2,2))
    withOutFileChecked(prefix+"matrices-big") {
      class Prog(val UID: Long = 5554L) extends Impl[DenseMatrix[Double]] {
        def main(): Rep[DenseMatrix[Double]] = {
          chain(holeM(v0, 0) * holeM(v1, 1) * holeM(v2, 2) * holeM(v3, 3) * holeM(v4, 4) * holeM(v5, 5) * holeM(v6, 6) * holeM(v7, 7) * holeM(v8, 8) * holeM(v9, 9))
        }
      }

      println((new Prog)(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9))
    }

  }
}
