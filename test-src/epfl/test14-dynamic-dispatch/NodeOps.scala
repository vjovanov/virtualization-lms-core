package scala.virtualization.lms
package epfl
package test14

import common._
//
// User interface
//
trait NodeOps extends Base {
  type Node[T]  
}

trait AggregateOps extends NodeOps {
  
  def aggregate[T: Manifest](t: Rep[Node[T]]): Rep[Node[T]]
  
}

trait JoinOps extends NodeOps {
  
  def join[R: Manifest, L: Manifest, Ret: Manifest](t: Rep[Node[L]], u: Rep[Node[R]]): Rep[Node[Ret]] 
}

//
// Expressions world
//

trait NodeExp extends BaseExp {
  
  trait NodeOps[T] {
    def next: Rep[T]
  } 
  
  type Node[T] = NodeOps[T]
  
  // converts 
  implicit def expToNode[T](in: Rep[Node[T]]): Node[T] = in.asInstanceOf[Node[T]] 
}

trait AggregateExp extends NodeExp {
  
  trait AggregateOps[T] extends Node[T] {    
    def next: Rep[T]
  }
  
  case class Agg[T](in: Exp[Node[T]]) extends Def[Node[T]] with AggregateOps[T] {
    def next: Rep[T] = in.next
  }
  
  def aggregate[T: Manifest](t: Rep[Node[T]]): Rep[Node[T]] = Agg[T](t) 
}

trait JoinExp extends NodeExp {
  
  trait JoinOps[R, L, Ret] extends Node[Ret] {
    def next: Rep[Ret]
  }
  
  case class Join[R, L, Ret : Manifest](r: Exp[Node[R]], l: Exp[Node[L]]) extends Def[Node[Ret]] with JoinOps[R, L, Ret] {
    def next: Rep[Ret] = {
      val x = r.next
      val y = l.next
      // if(matches ....) you need if then else here
      x.asInstanceOf[Rep[Ret]] // this cast is because I did not have the machinery to create righ type
    }
  }
  
  def join[R, L, Ret: Manifest](r: Rep[Node[R]], l: Rep[Node[L]]): Rep[Node[Ret]]  = Join[R, L, Ret](r,l)
}
