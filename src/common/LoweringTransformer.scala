package scala.virtualization.lms
package common

trait LoweringTransformer extends BaseFatExp with EffectExp with IfThenElseFatExp with LoopsFatExp { self =>

  trait LoweringTransformer extends WorklistTransformer { val IR: self.type = self }
  object lowering extends LoweringTransformer

  // ---------- Exp api

  implicit def toAfter[A: Manifest](x: Def[A]) =
    new { def atPhase(t: LoweringTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }
  implicit def toAfter[A](x: Exp[A]) =
    new { def atPhase(t: LoweringTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }

  // transform x to y at the *next* iteration of t.
  // note: if t is currently active, it will continue the current pass with x = x.
  // do we need a variant that replaces x -> y immediately if t is active?

  def transformAtPhase[A](x: Exp[A])(t: LoweringTransformer)(y: => Exp[A]): Exp[A] = {
    t.register(x)(y)
    x
  }
}

/*trait OptimizingTransformer extends BaseFatExp with EffectExp with IfThenElseFatExp with LoopsFatExp { self =>

  trait OptimizingTransformer extends WorklistTransformer { val IR: self.type = self }
  object optimizer extends LoweringTransformer

  // ---------- Exp api

  implicit def toAfter[A: Manifest](x: Def[A]) =
    new { def atPhase(t: LoweringTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }
  implicit def toAfter[A](x: Exp[A]) =
    new { def atPhase(t: LoweringTransformer)(y: => Exp[A]) = transformAtPhase(x)(t)(y) }

  // transform x to y at the *next* iteration of t.
  // note: if t is currently active, it will continue the current pass with x = x.
  // do we need a variant that replaces x -> y immediately if t is active?

  def transformAtPhase[A](x: Exp[A])(t: LoweringTransformer)(y: => Exp[A]): Exp[A] = {
    t.register(x)(y)
    x
  }
}*/ 