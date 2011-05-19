package scala.virtualization.lms
package epfl
package test9b

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}

trait FooBarExp extends Lattices {

  // -- expressions base

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def tp: Manifest[_] = manifest[T] //invariant position! but hey...
  }

  case class Const[+T:Manifest](x: T) extends Exp[T]

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    var sourceInfo = Thread.currentThread.getStackTrace // until we can get useful info out of the manifest
  }

  var nVars = 0
  def fresh[T:Manifest] = Sym[T] { nVars += 1; nVars -1 }

  // -- def and tp

  abstract class Def[+T] // operations (composite)

  case class TP[+T](sym: Sym[T], rhs: Def[T])

  var globalDefs: List[TP[Any]] = Nil

  def findDefinition[T](s: Sym[T]): Option[TP[T]] =
    globalDefs.find(_.sym == s).asInstanceOf[Option[TP[T]]]

  def findDefinition[T](d: Def[T]): Option[TP[T]] =
    globalDefs.find(_.rhs == d).asInstanceOf[Option[TP[T]]]

  def findOrCreateDefinition[T:Manifest](d: Def[T]): TP[T] =
    findDefinition[T](d).getOrElse {
      createDefinition(fresh[T], d)
    }

  def createDefinition[T](s: Sym[T], d: Def[T]): TP[T] = {
    val f = TP(s, d)
    globalDefs = globalDefs:::List(f)
    f
  }

  // --- api

  var scopeDefs: List[List[TP[Any]]] = Nil

  case class Block[T](stms: List[TP[Any]], e: Exp[T])

  protected implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] = {
    val f = createDefinition(fresh[T], d)
    scopeDefs = (scopeDefs.head:::List(f)) :: scopeDefs.tail
    f.sym
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).map(_.rhs)
      case _ =>
        None
    }
  }

  def reifyBlock[T](e: =>Exp[T]): Block[T] = {
    scopeDefs = Nil::scopeDefs
    val r = e
    val stms = scopeDefs.head
    scopeDefs = scopeDefs.tail
    Block(stms, r)
  }
  
  // --- analysis

  def syms(x: Any): List[Sym[Any]] = x match {
    case x: Sym[Any] => List(x)
    case x: Product => x.productIterator.toList.flatMap(syms)
    case _ => Nil
  }


  abstract class Formula


  case class Equation(val lhs: Sym[Any], val rhs: Formula)

  var globalEquations: List[Equation] = Nil
  
  
  
  implicit val setExp = setFiniteSet[Exp[Any]]
  implicit val setSym = setFiniteSet[Sym[Any]]
  implicit val mapSymSym = mapFiniteMap((k:Sym[Any]) => Set.empty[Sym[Any]])
  implicit val mapSymConst = mapFiniteMap((k:Sym[Any]) => AbsValNothing:AbsVal)
  
  abstract class AbsVal
  case object AbsValAny extends AbsVal
//  case class AbsValConst(x: Any) extends AbsVal
  case class AbsValPrim(x: Set[Exp[Any]]) extends AbsVal { override def toString = x.map { case Const(x) => x.toString case Sym(n) => "x"+n }.mkString("compute{",",","}") }
  case class AbsValRef(x: Set[Exp[Any]]) extends AbsVal { override def toString = x.map { case Const(x) => x.toString case Sym(n) => "x"+n }.mkString("{",",","}") }
  case object AbsValNothing extends AbsVal

  val absValLattice = new Lattice[AbsVal] {
    val setLattice2: Lattice[Set[Exp[Any]]] = setLattice
    def lub(x: AbsVal, y: AbsVal): AbsVal = (x,y) match {
      case (AbsValAny, _) => AbsValAny
      case (_, AbsValAny) => AbsValAny
      case (AbsValPrim(a), AbsValPrim(b)) => AbsValPrim(setLattice2.lub(a,b))
      case (AbsValPrim(a), _) => x
      case (_, AbsValPrim(b)) => y
      case (AbsValRef(a), AbsValRef(b)) => AbsValRef(setLattice2.lub(a,b))
      case (AbsValRef(a), _) => x
      case (_, AbsValRef(b)) => y
      case (_, _) => AbsValNothing
    }
    def glb(x: AbsVal, y: AbsVal): AbsVal = sys.error("not implemented")
  }

  type EnvVal = Map[Sym[Any], AbsVal]
  val envValLattice: Lattice[EnvVal] = mapLattice(mapSymConst, setLattice: Lattice[Set[Sym[Any]]], absValLattice)

  abstract class LiveVal
  case object LiveTrue extends LiveVal
  case object LiveMaybe extends LiveVal
  case object LiveFalse extends LiveVal

  val liveValLattice = new Lattice[LiveVal] {
    def lub(x: LiveVal, y: LiveVal): LiveVal = (x,y) match {
      case (LiveTrue, _) => LiveTrue
      case (_, LiveTrue) => LiveTrue
      case (LiveMaybe, _) => LiveMaybe
      case (_, LiveMaybe) => LiveMaybe
      case (_,_) => LiveFalse
    }
    def glb(x: LiveVal, y: LiveVal): LiveVal = sys.error("not implemented")
  }

  type AvailVal = Set[Sym[Any]]
  val availValLattice: Lattice[AvailVal] = setLattice

  abstract class Target
  case class Stack(s:Sym[Any]) extends Target { override def toString = "&x"+s.id }
  case class Heap(s:Sym[Any]) extends Target { override def toString = "malloc"+s.id }

  type HeapVal = Map[Target, Set[Target]]
  
  implicit val setTarget = setFiniteSet[Target]
  implicit val mapTargetTarget = mapFiniteMap((k:Target) => Set.empty[Target])
  
  val heapValLattice: Lattice[HeapVal] = mapLattice(mapTargetTarget, setLattice: Lattice[Set[Target]], setLattice: Lattice[Set[Target]])
  
/*  
  type AbstractValue = (LiveVal,AvailVal,EnvVal)
  val abstractLattice: Lattice[AbstractValue] = tup3Lattice(liveValLattice, availValLattice, envValLattice)  
  
  val Bottom: AbstractValue = (LiveFalse, Set.empty, Map.empty)
*/  

  type AbstractValue = (LiveVal,HeapVal)
  val abstractLattice: Lattice[AbstractValue] = tup2Lattice(liveValLattice, heapValLattice)  

  val Bottom: AbstractValue = (LiveFalse,Map.empty)



  
  case class Assignment(val lhs: Sym[_], val rhs: AbstractValue)
  
  var globalSolutions: List[Assignment] = Nil
  
  
  def createEquation(s: Sym[Any], rhs: Formula) = {
    assert(globalEquations.find(_.lhs == s).isEmpty)
    globalEquations = globalEquations :+ Equation(s, rhs)
    s
  }
  
  def solveAll() = {
    
    globalSolutions = globalEquations.map(e => Assignment(e.lhs, Bottom))
    var prev = globalSolutions
    do {
      prev = globalSolutions
      globalSolutions = (globalSolutions zip globalEquations) map { case (a,e) => 
        var res = interpret(e.lhs, e.rhs)
        // normalize...
        res = abstractLattice.lub(res, res) // e.g. for Map(a -> Nothing) ==> Map() //TODO
        assert(abstractLattice.lub(a.rhs, res) == res, "not monotonic: " + a.rhs + " ===> " + res)
        Assignment(a.lhs, res)
      }
    } while (prev != globalSolutions)
    
    // this is soup bla bla...
    var sp = soup
    do {
      sp = soup
      val res = globalDefs.flatMap { case TP(a,e) => foobar(a,e) }
      soup = (soup ++ res).sortBy(_.toString).distinct
    } while (sp != soup)
    
  }
  
  
  // -------- unused scratch stuff follows...
  
  var soup: List[List[Any]] = Nil
  
  def blks(b: Block[_]) = b match { case Block(stms,e) => stms.takeRight(1).map(_.sym) }
  
  def foobarb(b: Block[_]): List[List[Any]] = b match { case Block(stms, e) =>
    stms.flatMap(t => foobar(t.sym, t.rhs))
  }
  
  def foobar(s: Sym[Any], d: Def[Any]): List[List[Any]] = d match {
    case IfThenElse(c,a,b) if soup.contains(List("use", s)) =>
      foobarb(a) ++ foobarb(b) ++ List(List("use",blks(a)), List("use",blks(b), List("use",c)))

    case WhileDo(c,b) if soup.contains(List("use", s)) =>
      foobarb(c) ++ foobarb(b) ++ List(List("use",blks(b)), List("use",blks(c)))
        
    case _ =>
      if (soup.contains(List("end", s)))
        List(List("use", s))
      else
        Nil
  }
  
/*  
  var globalState: List[TP[Any]] = Nil

  def solveAllDirect() = {
    
    globalState = globalEquations.map(e => Assignment(e.lhs, Bottom))
    var prev = globalSolutions
    do {
      prev = globalSolutions
      globalSolutions = (globalSolutions zip globalEquations) map { case (a,e) => 
        val res = interpret(e.lhs, e.rhs)
        assert(abstractLattice.lub(a.rhs, res) == res, "not monotonic: " + a.rhs + " ===> " + res)
        Assignment(a.lhs, res)
      }
    } while (prev != globalSolutions)
    
    // this is bla bla...
    var sp = soup
    do {
      sp = soup
      val res = globalDefs.map { case TP(a,e) => foobar(a,e) }
      soup = (soup ++ res).sortBy(_.toString).distinct
    } while (sp != soup)
    
  }


  def abstractBlock[T](b: Block[T])(st: State): (State, Exp[T]) = b match {
    case Block(stms, e) =>
      var st1 = st
      stms.foreach { t => 
        st1 = abstractNode(t.sym, t.rhs)(st1)
      }
      (st1, e)
  }

  def abstractNode[T](s: Sym[T], d: Def[T])(st: State): State = d match {
    case IfThenElse(c,a,b) => 
      // split st
      val sta = createEquation(fresh[Any], stateCheckTrue(st,c))
      val stb = createEquation(fresh[Any], stateCheckFalse(st,c))
      createEquation(s, stateOrElse(abstractBlock(a)(sta),abstractBlock(b)(stb)))
    case WhileDo(c,b) =>

      val st0 = fresh[Any]
      val st1 = abstractBlock(c)(st0)

      // split st1

      val stb = createEquation(fresh[Any], stateCheckTrue(st1._1,st1._2))

      val st2 = abstractBlock(b)(stb)
      createEquation(st0, stateOrElse((st,Const()),st2))

      createEquation(s, stateCheckFalse(st1._1,st1._2))
/*      
    case VarInit(x:Sym[Any]) =>
      createEquation(s, stateNewVar(st, x))
    case VarAssign(v:Sym[Any], x:Sym[Any]) =>
      createEquation(s, stateVarAssign(st, v,x))
*/
    case _ =>
      createEquation(s, statePrim(st, d))
  }
*/


  
  // ------------------
  
  def getsol(s: Sym[Any]) = globalSolutions.find(_.lhs == s).get.rhs
  
  type PPoint = Any
  
  object Sol {
    def unapply(x: Sym[Any]): Option[PPoint] = Some(getsol(x))
  }
  

  def killvar(a: AbstractValue, s: Sym[Any]): AbstractValue = a match {
    case (z, store) => (z, store.updated(Stack(s), Set.empty))
  }

  def killvar(a: AbstractValue, s: Target): AbstractValue = a match {
    case (z, store) => (z, store.updated(s, Set.empty))
  }

  def allocvar(a: AbstractValue, s: Sym[Any]): AbstractValue = a match {
    case (z, store) => (z, store.updated(Stack(s), Set(Heap(s))))
  }

  def getvar(a: AbstractValue, s: Exp[Any]): Set[Target] = a match {
    case (z, store) => 
      s match {
        case s: Sym[Any] => store.getOrElse(Stack(s), Set.empty)
        case _ => Set.empty
      }
  }

  def getvar(a: AbstractValue, s: Target): Set[Target] = a match {
    case (z, store) => store.getOrElse(s, Set.empty)
  }

  def setvar(a: AbstractValue, s: Sym[Any], e: Set[Target]): AbstractValue = a match {
    case (z, store) => (z, store.updated(Stack(s), e))
  }

  def setvar(a: AbstractValue, s: Target, e: Set[Target]): AbstractValue = a match {
    case (z, store) => (z, store.updated(s, e))
  }



  // TODO: cse, dce
  /*
    cse: 
      initial assumption: all expressions compute same value (1 global partition)
      when discovering one that doesn't, split partition
  */

  def interpret(s: Sym[Any], f: Formula): AbstractValue = f match {
    case StateInit() => (LiveTrue, Map.empty)
    case StateRef(Sol(a:AbstractValue)) => a
    case StateOrElse((Sol(a:AbstractValue),u),(Sol(b:AbstractValue),v)) => 
      abstractLattice.lub(a, b) // ignore result val for now ...

    case StateCheckTrue(Sol(a:AbstractValue),c) => a
    case StateCheckFalse(Sol(a:AbstractValue),c) => a

    case StatePrim(Sol(a:AbstractValue),VarInit(x)) => 
      setvar(a,s,getvar(a,x))

    case StatePrim(Sol(a:AbstractValue),VarAssign(v: Sym[Any],x)) => 
      setvar(a,v,getvar(a,x))


    case StatePrim(Sol(a:AbstractValue),PtrNull()) => 
      killvar(a,s)

    case StatePrim(Sol(a:AbstractValue),PtrAlloc()) => 
      allocvar(killvar(a,s), s)

    case StatePrim(Sol(a:AbstractValue),PtrDeref(x)) =>  // right(s,x): (s,t) for all (&x,u),(u,t) ∈ σ
      setvar(a,s,getvar(a,x) flatMap (u => getvar(a,u)))

    case StatePrim(Sol(a:AbstractValue),PtrUpdate(x,y)) => // left(x,y)   *x = y
      val xderef = getvar(a,x)
      if (xderef.isEmpty) a
      else {
        val yderef = getvar(a,y)        
        if (yderef.isEmpty) {
          var a2 = a
          for (s <- xderef) a2 = killvar(a2,s)
          a2
        } else {
          var a2 = a
          for (s <- xderef) {
            a2 = killvar(a2,s)
            //for (t <- yderef)
              a2 = setvar(a2, s, yderef)
          }
          a2
        }
      }

      // σ                                      if {s|(&x,s)∈σ}=∅
      // ∪{σ↓s|(&x,s)∈σ}                        if {s|(&x,s)∈σ}≠∅ ∧ {t|(&y,t)∈σ}=∅
      // ∪{σ↓s ∪ {(s,t)}|(&x,s),(&y,t)∈σ}       otherwise
      
    case StatePrim(Sol(a:AbstractValue),x) => a
      
    case _ => getsol(s) // just return previous value...
  }

/*

//  def islive(s: Sym[Any]) = 
  
  def getexp(a: AbstractValue, e: Exp[Any]): AbsVal = e match {
    case Const(x) => AbsValRef(Set(e))
    case x: Sym[Any] => a match { case (z, avail, env) => env.getOrElse(x, AbsValNothing) }
  }

  def setvar(a: AbstractValue, e: Sym[Any], x: AbsVal) = a match {
    case (z, avail, env) => (z, avail ++ globalDefs.collect({case tp if syms(tp.rhs).contains(e) => tp.sym}).toSet, env.map({
//      case (s, AbsValRef(set)) if set.contains(e) => (s,AbsValAny)  hmmmmm.....
      case p => p
    }).updated(e, x))
  }

  def setavail(a: AbstractValue, e: Sym[Any]) = a match {
    case (z, avail, env) => (z, avail diff Set(e), env)
  }

  def isavail(a: AbstractValue, e: Def[Any]) = a match {
    case (z, avail, env) => globalDefs.collectFirst({ case TP(s, `e`) if !avail.contains(s) => s }).toSet
  }

  def cond(a: AbstractValue, c: Exp[Any], z: Exp[Any]) = getexp(a, c) match { // c <= z?
    case AbsValAny => false
    case AbsValPrim(set) => false
    case AbsValRef(set) => set == Set(z)
    case AbsValNothing => true
  }
  
  def live(a: AbstractValue) = a match { // live > false
    case (LiveFalse, _, _) => false
    case (_, _, _) => true
  }


  // TODO: cse, dce
  /*
    cse: 
      initial assumption: all expressions compute same value (1 global partition)
      when discovering one that doesn't, split partition
  */

  def interpret(s: Sym[Any], f: Formula): AbstractValue = f match {
    case StateInit() => (LiveTrue, Set.empty, Map.empty)
    case StateRef(Sol(a:AbstractValue)) => a
    case StateOrElse((Sol(a:AbstractValue),u),(Sol(b:AbstractValue),v)) => 
      abstractLattice.lub(a, b) // ignore result val for now ...
      //setvar(abstractLattice.lub(a, b), s, absValLattice.lub(getexp(a,u),getexp(b,v))) // no: if one branch is unreachable, don't take result!
    
    case StatePrim(Sol(a:AbstractValue),VarInit(x)) => if (live(a)) setvar(a,s,getexp(a,x)) else Bottom
    case StatePrim(Sol(a:AbstractValue),VarAssign(v: Sym[Any],x)) => if (live(a)) setvar(a,v,getexp(a,x)) else Bottom
    
    case StateCheckTrue(Sol(a:AbstractValue),c) => if (live(a) && !cond(a,c,Const(false))) // should use maybe?
      c match { case s:Sym[Any] => setvar(a,s,getexp(a,Const(true))) case _ => a } else Bottom
    case StateCheckFalse(Sol(a:AbstractValue),c) => if (live(a) && !cond(a,c,Const(true))) 
      c match { case s:Sym[Any] => setvar(a,s,getexp(a,Const(false))) case _ => a } else Bottom
    
    case StatePrim(Sol(a:AbstractValue),x) => if (live(a)) 
      isavail(a, x) match {  // if rhs is available, reuse it!
        case set if set.nonEmpty => 
        
          val in = set.toList.map(s2=>getexp(a,s2)).reduce(absValLattice.glb) // 
          setavail(setvar(a, s, in),s)
        
        // possible inputs: intersection of all possible reused defs

        // increase dead -> reduce reused defs -> increase inputs (but: reused defs = empty => inputs = infty!)
        
        // trigger: none available --> need to compute
        
        case _ => setavail(setvar(a,s,AbsValRef(Set(s))),s) // <--- is that monotone ??? no: things can become unavailable...
      } else Bottom
    case _ => getsol(s) // just return previous value...
  }
  
*/  

  // forward dataflow analyis
  // alternative: effect abstraction. but: hmmm ...
  
  case class StateOrElse(a: (Sym[Any], Exp[Any]), b: (Sym[Any], Exp[Any])) extends Formula
  case class StateRef(a: Sym[Any]) extends Formula
  case class StatePrim(a: Sym[Any], rhs: Def[Any]) extends Formula
  case class StateInit() extends Formula
  
  case class StateCheckTrue(s: Sym[Any], c: Exp[Boolean]) extends Formula
  case class StateCheckFalse(s: Sym[Any], c: Exp[Boolean]) extends Formula

  type State = Sym[Any]
  
  def stateRef(a: State): Formula = StateRef(a)

  def stateOrElse(a: (State, Exp[Any]), b: (State, Exp[Any])): Formula = StateOrElse(a,b)
  def stateAndAlso(a: State, b: State): Formula = sys.error("not implemented")
  
  def stateInit: State = createEquation(fresh[Any], StateInit())

/*
  def stateNewVar(a: Sym[Any], x: Sym[Any]): Formula = StateNewVar(a, x)
  def stateVarAssign(a: Sym[Any], v: Sym[Any], x: Sym[Any]): Formula = StateVarAssign(a, v, x)
*/
  def statePrim(a: State, rhs: Def[Any]): Formula = StatePrim(a, rhs)

  def stateCheckTrue(a: State, c: Exp[Boolean]): Formula = StateCheckTrue(a, c)
  def stateCheckFalse(a: State, c: Exp[Boolean]): Formula = StateCheckFalse(a, c)

  def abstractBlock[T](b: Block[T])(st: State): (State, Exp[T]) = b match {
    case Block(stms, e) =>
      var st1 = st
      stms.foreach { t => 
        st1 = abstractNode(t.sym, t.rhs)(st1)
      }
      (st1, e)
  }

  def abstractNode[T](s: Sym[T], d: Def[T])(st: State): State = d match {
    case IfThenElse(c,a,b) => 
      // split st
      val sta = createEquation(fresh[Any], stateCheckTrue(st,c))
      val stb = createEquation(fresh[Any], stateCheckFalse(st,c))
      createEquation(s, stateOrElse(abstractBlock(a)(sta),abstractBlock(b)(stb)))
    case WhileDo(c,b) =>
    
      val st0 = fresh[Any]
      val st1 = abstractBlock(c)(st0)

      // split st1

      val stb = createEquation(fresh[Any], stateCheckTrue(st1._1,st1._2))

      val st2 = abstractBlock(b)(stb)
      createEquation(st0, stateOrElse((st,Const()),st2))

      createEquation(s, stateCheckFalse(st1._1,st1._2))
/*      
    case VarInit(x:Sym[Any]) =>
      createEquation(s, stateNewVar(st, x))
    case VarAssign(v:Sym[Any], x:Sym[Any]) =>
      createEquation(s, stateVarAssign(st, v,x))
*/
    case _ =>
      createEquation(s, statePrim(st, d))
  }
  
  
  
  



  // --- codegen

  var nesting = 0
  var indent = true

  def quote(s: Any): String = s match {
    case s @ Sym(id) => "x" + id
    case Const(c) => c.toString
  }

  def emitValDef[T](s: Sym[T], rhs: String, more: Boolean = false) = {
    emitPlain("val " + quote(s) + " = " + rhs, more)
  }

  def emitPlain(s: String, more: Boolean = false) = {
    if (indent) print(" " * (nesting * 2))
    if (more) print(s) else println(s)
    indent = !more
  }

  def emitNode[T](s: Sym[T], d: Def[T]): Unit = d match {
    case IfThenElse(c,a,b) => 
      emitValDef(s, "if (" + quote(c) + ") ", true)
      emitBlock(a, true)
      emitPlain(" else ", true)
      emitBlock(b)
    case VarInit(x) => 
      emitPlain("var " + quote(s) + " = " + quote(x))
    case VarAssign(v,x) => 
      emitValDef(s, "(" + quote(v) + " = " + quote(x) + ")")
    case WhileDo(c,b) =>
      emitValDef(s, "while (", true)
      emitBlock(c, true)
      emitPlain(") ", true)
      emitBlock(b)
    case _ =>
      emitValDef(s, "UNKNOWN: " + d)
  }

  def emitBlock[T](a: Block[T], more: Boolean = false) = a match {
    case Block(stms, e) =>
      emitPlain("{"/*}*/, false)
      nesting += 1
      stms foreach { t => emitNode(t.sym, t.rhs) }
      emitPlain(quote(e))
      nesting -= 1
      emitPlain(/*{*/"}", more)
  }


  // --- client dsl

  case class IfThenElse[T](c: Exp[Boolean], a: Block[T], b: Block[T]) extends Def[T]
  case class WhileDo(c: Block[Boolean], b: Block[Unit]) extends Def[Unit]

  def ifThenElse[T:Manifest](c: Exp[Boolean], a: =>Exp[T], b: =>Exp[T]): Exp[T] = {
    IfThenElse(c, reifyBlock(a), reifyBlock(b))
  }
  
  def whileDo(c: =>Exp[Boolean], b: =>Exp[Unit]): Exp[Unit] = {
    WhileDo(reifyBlock(c), reifyBlock(b))
  }
  

  case class VarInit[T](x: Exp[T]) extends Def[T]
  case class VarAssign[T](v: Exp[T], x: Exp[T]) extends Def[Unit]

  def varInit[T:Manifest](x: Exp[T]): Exp[T] = VarInit(x)
  def varAssign[T](v: Exp[T], x: Exp[T]): Exp[Unit] = VarAssign(v, x)
  
  case class Plus[T](a: Exp[T], b: Exp[T]) extends Def[T]
  case class Minus[T](a: Exp[T], b: Exp[T]) extends Def[T]
  case class Times[T](a: Exp[T], b: Exp[T]) extends Def[T]
  case class Div[T](a: Exp[T], b: Exp[T]) extends Def[T]
  
  def plus[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = Plus(a,b)
  def minus[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = Minus(a,b)
  def times[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = Times(a,b)
  def div[T:Manifest:Numeric](a: Exp[T], b: Exp[T]): Exp[T] = Div(a,b)
  
  case class Equal[T](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  case class NotEqual[T](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  
  def equal[T,U](a: Exp[T], b: Exp[U]): Exp[Boolean] = Equal(a,b)
  def notEqual[T,U](a: Exp[T], b: Exp[U]): Exp[Boolean] = NotEqual(a,b)
  
  case class LessThan[T:Ordering](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  case class LessThanEqual[T:Ordering](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  case class GreaterThan[T:Ordering](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  case class GreaterThanEqual[T:Ordering](a: Exp[T], b: Exp[T]) extends Def[Boolean]
  
  def lessThan[T:Ordering](a: Exp[T], b: Exp[T]): Exp[Boolean] = LessThan(a,b)
  def lessThanEqual[T:Ordering](a: Exp[T], b: Exp[T]): Exp[Boolean] = LessThanEqual(a,b)
  def greaterThan[T:Ordering](a: Exp[T], b: Exp[T]): Exp[Boolean] = GreaterThan(a,b)
  def greaterThanEqual[T:Ordering](a: Exp[T], b: Exp[T]): Exp[Boolean] = GreaterThanEqual(a,b)

  trait Ptr[T]
  
  case class PtrNull[T]() extends Def[Ptr[T]]
  case class PtrAlloc[T]() extends Def[Ptr[T]]
  case class PtrDeref[T](x: Exp[Ptr[T]]) extends Def[T]
  case class PtrUpdate[T](x: Exp[Ptr[T]], y: Exp[T]) extends Def[Unit]
  
  def pointerNull[T:Manifest](): Exp[Ptr[T]] = PtrNull[T]()
  def pointerAlloc[T:Manifest](): Exp[Ptr[T]] = PtrAlloc[T]()
  def pointerDeref[T:Manifest](x: Exp[Ptr[T]]): Exp[T] = PtrDeref[T](x)
  def pointerUpdate[T:Manifest](x: Exp[Ptr[T]], y: Exp[T]): Exp[Unit] = PtrUpdate[T](x, y)
}


trait Lattices {
  
  trait Lattice[T] {
    def lub(x: T, y: T): T
    def glb(x: T, y: T): T
  }
  
  trait Monoid[T] {
    def zero: T
    def plus(x: T, y: T): T
  }
  
  trait Collection[C,T] {
    def foreach(xs: C)(f: T => Unit): Unit
  }
  
  def transform[C,T,U](xs: C, f: T => U)(implicit c: Collection[C,T], m: Monoid[U]): U = {
    var it = m.zero
    c.foreach(xs) { x =>
      it = m.plus(it, f(x))
    }
    it
  }
  
  
  implicit val intMonoid = new Monoid[Int] {
    def zero = 0
    def plus(x: Int, y: Int) = x + y
  }
  
  implicit def listMonoid[T] = new Monoid[List[T]] {
    def zero = Nil
    def plus(x: List[T], y: List[T]) = x ::: y
  }
  
  implicit def iterableCollection[T] = new Collection[Iterable[T],T] {
    def foreach(xs: Iterable[T])(f: T => Unit) = xs.foreach(f)
  }
  
  trait Equality[T] {
    def isEqual(x: T, y: T): Boolean
  }
  
  implicit def regularEquality[T] = new Equality[T] {
    def isEqual(x: T, y: T) = x == y
  }
  
  trait FiniteSet[C,T] extends Collection[C,T] {
    def oe: Equality[T]
    def empty: C
    def size(xs: C): Int
    def contains(xs: C, y: T): Boolean
    def union(x: C, y: C): C
    def intersect(x: C, y: C): C
  }
  
  trait FiniteMap[M,KS,K,V] {
    def os: FiniteSet[KS,K]
    def oe: Equality[V]
    def default(k: K): V
    
    def domain(m: M): KS
    def get(m: M, k: K): V
    
    def empty: M
    def create(ks: KS)(f: K => V): M
  }
  
  implicit def setFiniteSet[T](implicit ie: Equality[T]) = new FiniteSet[Set[T],T] {
    def oe = ie
    def empty = Set.empty[T]
    def size(xs: Set[T]) = xs.size
    def contains(xs: Set[T], y: T) = xs.find(x => ie.isEqual(x,y)).nonEmpty
    def foreach(xs: Set[T])(f: T => Unit) = xs.foreach(f)
    def union(x: Set[T], y: Set[T]) = x.union(y)
    def intersect(x: Set[T], y: Set[T]) = x.intersect(y)
  }
  
  implicit def mapFiniteMap[K,V](f: K=>V)(implicit ie1: Equality[K], ie2: Equality[V]) = new FiniteMap[Map[K,V], Set[K], K, V] {
    val os = setFiniteSet(ie1)
    val oe = ie2
    def default(k: K) = f(k)
    
    def domain(m: Map[K,V]) = m.keySet
    def get(m: Map[K,V], k: K) = m.getOrElse(k, default(k))
    
    def empty = Map.empty[K,V]
    
    def create(ks: Set[K])(f: K => V) = {
      var m = Map.empty[K,V]
      os.foreach(ks) { k =>
        val v = f(k)
        if (!oe.isEqual(v,default(k)))
          m = m.updated(k,v)
      }
      m
    }
  }
  
  def mapPlus[M,KS,K,V](a: M, b: M)(implicit om: FiniteMap[M,KS,K,V], os: Monoid[KS], ov: Monoid[V]) = {
    val keys = os.plus(om.domain(a), om.domain(b))
    om.create(keys) { k => ov.plus(om.get(a,k), om.get(b,k)) }
  }
  
  
  implicit val intLattice = new Lattice[Int] {
    def lub(x: Int, y: Int) = math.max(x,y)
    def glb(x: Int, y: Int) = math.min(x,y)
  }
  
  implicit def setLattice[C,T](implicit il: FiniteSet[C,T]) = new Lattice[C] { // TODO: use instead of Set[T]
    def lub(x: C, y: C) = il.union(x,y)
    def glb(x: C, y: C) = il.intersect(x,y)
  }

  implicit def setLatticeInv[C,T](implicit il: FiniteSet[C,T]) = new Lattice[C] { // TODO: use instead of Set[T]
    def lub(x: C, y: C) = il.intersect(x,y)
    def glb(x: C, y: C) = il.union(x,y)
  }

  implicit def mapLattice[M,KS,K,V](implicit im: FiniteMap[M,KS,K,V], ilk: Lattice[KS], ilv: Lattice[V]) = new Lattice[M] {
    def lub(x: M, y: M) = im.create(ilk.lub(im.domain(x), im.domain(y))) { k => ilv.lub(im.get(x,k), im.get(y,k)) }
    def glb(x: M, y: M) = im.create(ilk.glb(im.domain(x), im.domain(y))) { k => ilv.glb(im.get(x,k), im.get(y,k)) }
  }

  implicit def tup2Lattice[A,B](implicit i1: Lattice[A], i2: Lattice[B]) = new Lattice[(A,B)] {
    def lub(x: (A,B), y: (A,B)) = (i1.lub(x._1, y._1), i2.lub(x._2, y._2))
    def glb(x: (A,B), y: (A,B)) = (i1.glb(x._1, y._1), i2.glb(x._2, y._2))
  }

  implicit def tup3Lattice[A,B,C](implicit i1: Lattice[A], i2: Lattice[B], i3: Lattice[C]) = new Lattice[(A,B,C)] {
    def lub(x: (A,B,C), y: (A,B,C)) = (i1.lub(x._1, y._1), i2.lub(x._2, y._2), i3.lub(x._3, y._3))
    def glb(x: (A,B,C), y: (A,B,C)) = (i1.glb(x._1, y._1), i2.glb(x._2, y._2), i3.glb(x._3, y._3))
  }


  def testXX = {
/*    
    implicit val grr = mapFiniteMap((k:String) => Set[Int]())
    
    val m1 = Map("x1" -> Set(3,4), "x2" -> Set(1))
    val m2 = Map("x2" -> Set(5,6), "x3" -> Set(2))
    
    val m3 = mapLattice.lub(m1,m2)
    
    println(m3)
*/

  }

  
}





trait FooBarOptExp extends FooBarExp {
  //---
}


trait FooBarLiftExp extends EmbeddedControls with FooBarExp with OverloadHack {
  
  type Rep[+T] = Exp[T]
  
  implicit def unit[T:Manifest](x: T): Rep[T] = Const(x)
  
  implicit def numericOps[T:Manifest:Numeric](x: Exp[T]) = new {
    def +(y: Exp[T]) = plus(x,y)
    def -(y: Exp[T]) = minus(x,y)
    def *(y: Exp[T]) = times(x,y)
    def /(y: Exp[T]) = div(x,y)
  }
/*  
  def infix_+[T:Manifest:Numeric](a: Exp[T], b: Exp[T]) = plus(a,b)
  def infix_-[T:Manifest:Numeric](a: Exp[T], b: Exp[T]) = minus(a,b)
  def infix_*[T:Manifest:Numeric](a: Exp[T], b: Exp[T]) = times(a,b)
  def infix_/[T:Manifest:Numeric](a: Exp[T], b: Exp[T]) = div(a,b)
*/  

  implicit def orderingOps[T:Manifest:Ordering](x: Exp[T]) = new {
    def <(y: Exp[T]) = lessThan(x,y)
    def <=(y: Exp[T]) = lessThanEqual(x,y)
    def >(y: Exp[T]) = greaterThan(x,y)
    def >=(y: Exp[T]) = greaterThanEqual(x,y)
  }


  def malloc[T:Manifest]: Rep[Ptr[T]] = pointerAlloc()
  def NULL[T:Manifest]: Rep[Ptr[T]] = pointerNull()
  class ptrOps[T:Manifest](x: Rep[Ptr[T]]) {
    def deref_=(y: Rep[T]): Unit = pointerUpdate(x, y)
    def deref: Rep[T] = pointerDeref(x)
  }
  implicit def ptr_ops[T:Manifest](x: Rep[Ptr[T]]) = new ptrOps[T](x)
  
  
  def __newVar[T:Manifest](x: T) = varInit(unit(x))
  def __newVar[T](x: Rep[T])(implicit o: Overloaded1, mT: Manifest[T]) = varInit(x)
  
  def __assign[T:Manifest](lhs: Rep[T], rhs: T) = varAssign(lhs, unit(rhs))
  def __assign[T](lhs: Rep[T], rhs: Rep[T])(implicit o: Overloaded1, mT: Manifest[T]) = varAssign(lhs, rhs)
  
  def __ifThenElse[T:Manifest](cond: => Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = ifThenElse(cond, thenp, elsep)
  override def __ifThenElse[T](cond: => Boolean, thenp: => T, elsep: => T): T = cond match { // HACK -- bug in scala-virtualized
    case true => thenp
    case false => elsep
  }
  
  def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) = whileDo(cond, body)
  
  def __equal[A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = equal(a,b)
  def __equal[A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = equal(a, unit(b))
  def __equal[A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = equal(unit(a), b)

  def infix_![A,B](a: Rep[A], b: Rep[B])(implicit o: Overloaded1, mA: Manifest[A], mB: Manifest[B]) : Rep[Boolean] = notEqual(a,b)
  def infix_![A,B](a: Rep[A], b: B)(implicit o: Overloaded4, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = notEqual(a, unit(b))
  def infix_![A,B](a: A, b: Rep[B])(implicit o: Overloaded5, mA: Manifest[A], mB: Manifest[B]): Rep[Boolean] = notEqual(unit(a), b)
  
}


class TestFoobarExp extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test9-"
  
  trait DSL extends FooBarExp with FooBarLiftExp with FooBarOptExp {
    def test(x: Rep[Int]): Rep[Any]
  }
  trait Impl extends DSL {

    val x = fresh[Int]
    val y = reifyBlock(test(x))
    
    println("--- code")
    emitPlain("object FooBar extends (Int => Any) {"/*}*/)
    emitPlain("def apply("+quote(x)+": Int) = ", true)
    emitBlock(y)
    emitPlain(/*{*/"}")

    
    val st0 = stateInit
    println("start: " + st0)
    val st1 = abstractBlock(y)(st0)
    println("end: " + st1)
    
    println("--- equations")
    globalEquations.foreach(println)
    
    soup = List(List("start", st0), List("end", st1._1))
    
    solveAll()
    
    println("--- solutions")
    globalSolutions.foreach(println)

    println("--- soup")
    soup.foreach(println)
    
  }
  
  def testLattice = {
    (new FooBarExp {} ).testXX
  }
  
  def testFoobar1 = {
    withOutFile(prefix+"foobarexp1") {
      trait Prog extends DSL {
        def power(b: Rep[Double], x: Int): Rep[Double] = 
          if (x == 0) 1.0
          else if ((x&1) == 0) { val y = power(b, x/2); y * y }
          else b * power(b, x - 1)
          
        def test(x: Rep[Int]) = {
          power(x.asInstanceOf[Rep[Double]], 8)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"foobarexp1")
  }

  def testFoobar2 = {
    withOutFile(prefix+"foobarexp2") {
      {trait Prog extends DSL {
        def test(input: Rep[Int]) = {
          var n = 0;
          var x,y,p,q: Rep[Ptr[Any]] = NULL[Any];
          x = malloc; 
          y = malloc;
          x deref_= NULL;
          y deref_= y;
          n = input;
          while (n > 0) {
            p = malloc; 
            q = malloc;
            p deref_= x;
            q deref_= y;
            x = p; 
            y = q;
            n = n-1;
          }
        }
      }
      new Prog with Impl}
    }
    assertFileEqualsCheck(prefix+"foobarexp2")
  }
}