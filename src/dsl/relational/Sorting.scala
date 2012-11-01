package dsl.relational

import scala.virtualization.lms.common._

class CostedNode

class ImplementedNode

case class StagingContext

trait SortingOps extends ScalaOpsPkg {

  def sort[T: Manifest](coll: Rep[Array[T]]): Rep[Array[T]]

}

object SortingExp {
  def truePredicate = true
  
  def optional[T](p: => Boolean)(body: => T) = if (p) Some(body) else None
}

/** 
 * TODO find a way not to introduce implementation Ops into the DSL. Currently the Exp part of the DSL must contain all the modules that are used in implementation.
 */
trait SortingExp extends SortingOps with ScalaOpsPkgExp  {
  
  case class Sort[T: Manifest: Ordering](coll: Exp[Array[T]]) extends Def[Array[T]] {
    def cost: Long = 0
    
    def implementations[T: Manifest: Ordering]: List[scala.Function2[Exp[Array[T]], StagingContext, Option[Exp[Array[T]]]]] = scala.collection.immutable.List(
      (a, b) => { // stupid sort
        SortingExp.optional(SortingExp.truePredicate) {
          
        var i: Rep[Int] = unit(0) // bad selection sort
        while (i < a.length) {
          var j: Rep[Int] = i + 1
          while (j < a.length) {
            if (a(i) > a(j)) {
              var tmp = a(i)
              a(i) = a(j)
              a(j) = tmp
            }
            j += 1
          }
          i += 1
        }
        a
       }
      },
      
      (a, b) => SortingExp.optional(true) { // selection-sort
        var i: Rep[Int] = unit(0)
        while (i < a.length) {
          var j: Rep[Int] = i + 1
          var max = j
          while (j < a.length) {
            if (a(j) > a(max)) 
              max = a(j)
            j += 1
          }
          if (a(i) > a(max)) {
            var tmp = a(i)
            a(i) = a(max)
            a(max) = tmp
          }
          i += 1
        }
        a
      })
  }

  def sort[T: Manifest: Ordering](coll: Exp[Array[T]]): Exp[Array[T]] = Sort(coll)

  // can all operations implement 

}