package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.virtualization.lms.internal.GenericNestedCodegen
import collection.mutable.ArrayBuffer

trait ArrayBufferOps extends Base {

  object ArrayBuffer {
    def apply[A: Manifest](xs: Rep[A]*) = arraybuffer_new(xs.toSeq)(manifest[A], null)
  }

  implicit def repToArrayBufferOps[A: Manifest](l: Rep[ArrayBuffer[A]]) = new ArrayBufferOpsCls(l)

  class ArrayBufferOpsCls[A: Manifest](l: Rep[ArrayBuffer[A]]) {
    def +=(e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)
    def ++=(e: Rep[Seq[A]])(implicit pos: SourceContext) = arraybuffer_appendSeq(l, e)
    def foreach(block: Rep[A] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit] = arraybuffer_foreach(l, block)
    def mkString(sep: Rep[String] = unit(""))(implicit pos: SourceContext) = arraybuffer_mkstring(l, sep)
    def append(l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext) = arraybuffer_append(l, e)
    def clear()(implicit pos: SourceContext) = arraybuffer_clear(l)
    def toArray(implicit pos: SourceContext) = arraybuffer_toarray(l)
    def toSeq(implicit pos: SourceContext) = arraybuffer_toseq(l)
    def size(implicit pos: SourceContext) = arraybuffer_size(l)
  }

  def arraybuffer_new[A: Manifest](xs: Seq[Rep[A]])(implicit pos: SourceContext): Rep[ArrayBuffer[A]]
  def arraybuffer_mkstring[A: Manifest](l: Rep[ArrayBuffer[A]], sep: Rep[String])(implicit pos: SourceContext): Rep[String]
  def arraybuffer_append[A: Manifest](l: Rep[ArrayBuffer[A]], e: Rep[A])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_appendSeq[A: Manifest](l: Rep[ArrayBuffer[A]], xs: Rep[Seq[A]])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_clear[A: Manifest](l: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_toarray[A: Manifest](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Array[A]]
  def arraybuffer_toseq[A: Manifest](x: Rep[ArrayBuffer[A]])(implicit pos: SourceContext): Rep[Seq[A]]
  def arraybuffer_foreach[T: Manifest](x: Rep[ArrayBuffer[T]], block: Rep[T] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
  def arraybuffer_size[T: Manifest](x: Rep[ArrayBuffer[T]])(implicit pos: SourceContext): Rep[Int]
}

trait ArrayBufferOpsExp extends ArrayBufferOps with EffectExp {
  case class ArrayBufferNew[A: Manifest](xs: Seq[Exp[A]]) extends Def[ArrayBuffer[A]] {
    val mA = manifest[A]
  }
  case class ArrayBufferMkString[A: Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String]) extends Def[String]
  case class ArrayBufferAppend[A: Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A]) extends Def[Unit]
  case class ArrayBufferAppendSeq[A: Manifest](l: Exp[ArrayBuffer[A]], e: Exp[Seq[A]]) extends Def[Unit]
  case class ArrayBufferClear[A: Manifest](l: Exp[ArrayBuffer[A]]) extends Def[Unit]
  case class ArrayBufferSize[A: Manifest](l: Exp[ArrayBuffer[A]]) extends Def[Int]
  case class ArrayBufferToArray[A: Manifest](x: Exp[ArrayBuffer[A]]) extends Def[Array[A]]
  case class ArrayBufferForeach[A: Manifest](x: Exp[ArrayBuffer[A]], i: Sym[A], block: Block[Unit]) extends Def[Unit]
  case class ArrayBufferToSeq[A: Manifest](x: Exp[ArrayBuffer[A]]) extends Def[Seq[A]]

  def arraybuffer_new[A: Manifest](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(ArrayBufferNew(xs))
  def arraybuffer_mkstring[A: Manifest](l: Exp[ArrayBuffer[A]], sep: Exp[String])(implicit pos: SourceContext) = ArrayBufferMkString(l, sep)
  def arraybuffer_append[A: Manifest](l: Exp[ArrayBuffer[A]], e: Exp[A])(implicit pos: SourceContext) = reflectWrite(l)(ArrayBufferAppend(l, e))
  def arraybuffer_appendSeq[A: Manifest](l: Exp[ArrayBuffer[A]], xs: Rep[Seq[A]])(implicit pos: SourceContext): Rep[Unit] = reflectWrite(l)(ArrayBufferAppendSeq(l, xs))
  def arraybuffer_clear[A: Manifest](l: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = reflectWrite(l)(ArrayBufferClear(l))
  def arraybuffer_toarray[A: Manifest](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferToArray(x)
  def arraybuffer_toseq[A: Manifest](x: Exp[ArrayBuffer[A]])(implicit pos: SourceContext) = ArrayBufferToSeq(x)
  def arraybuffer_foreach[T: Manifest](l: Rep[ArrayBuffer[T]], block: Rep[T] => Rep[Unit])(implicit pos: SourceContext): Rep[Unit] = {
    val x = fresh[T]
    val b = reifyEffects(block(x))
    reflectEffect(ArrayBufferForeach(l, x, b), summarizeEffects(b).star)
  }
  def arraybuffer_size[T: Manifest](x: Rep[ArrayBuffer[T]])(implicit pos: SourceContext): Rep[Int] = ArrayBufferSize(x)
  //////////////
  // mirroring

  override def syms(e: Any): List[Sym[Any]] = e match {
    case ArrayBufferForeach(a, x, body) => syms(a) ::: syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ArrayBufferForeach(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case ArrayBufferForeach(a, x, body) => freqNormal(a) ::: freqHot(body)
    case _ => super.symsFreq(e)
  }

  /*
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(ArrayBufferMkString(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferMkString(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(ArrayBufferAppend(l,r), u, es) => reflectMirrored(Reflect(ArrayBufferAppend(f(l),f(r)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
*/
}

trait BaseGenArrayBufferOps extends GenericNestedCodegen {
  val IR: ArrayBufferOpsExp
  import IR._
}

trait ScalaGenArrayBufferOps extends BaseGenArrayBufferOps with ScalaGenEffect {
  val IR: ArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a @ ArrayBufferNew(xs) => emitValDef(sym, "scala.collection.mutable.ArrayBuffer[" + remap(a.mA) + "](" + (xs map { quote }).mkString(",") + ")")
    case ArrayBufferMkString(l, sep) => emitValDef(sym, quote(l) + ".mkString(" + quote(sep) + ")")
    case ArrayBufferAppend(l, e) => emitValDef(sym, quote(l) + " += " + quote(e))
    case ArrayBufferAppendSeq(l, e) => emitValDef(sym, quote(l) + " ++= " + quote(e))
    case ArrayBufferClear(l) => emitValDef(sym, quote(l) + ".clear()")
    case ArrayBufferToArray(x) => emitValDef(sym, quote(x) + ".toArray")
    case ArrayBufferToSeq(x) => emitValDef(sym, quote(x) + ".toSeq")
    case ArrayBufferForeach(a, x, block) =>
      stream.println("val " + quote(sym) + " = " + quote(a) + ".foreach{")
      stream.println(quote(x) + " => ")
      emitBlock(block)
      stream.println(quote(getBlockResult(block)))
      stream.println("}")
    case ArrayBufferSize(l) => emitValDef(sym, quote(l) + ".size")
    case _ => super.emitNode(sym, rhs)
  }
}

