package dsl.relational

import scala.virtualization.lms.common._

case class Targets
trait CostNode {
  def cost: Long
}

/**
 * This module builds a toy cost based optimizer for the relational algebra. 
 */
//trait RelationalOps { this: IterableOps with TupleOps =>
  
  /*
   * Since this is exposed to the user we would put the  
   */
//  def join[V1: Manifest, V2: Manifest](v1: Rep[Iterable[Struct]], v2: Rep[Iterable[Struct]]): Rep[Iterable[Struct]]

//}

/*trait RelationalExp extends RelationalOps with BaseExp { 
  
  
  def join[V1: Manifest, V2: Manifest](v1: Rep[Iterable[Struct]], v2: Rep[Iterable[Struct]]): Rep[Iterable[Struct]] = 
    new EquiJoin(v1, v2)
  
  case class EquiJoin(v1: Rep[Iterable[Struct]], v2: Rep[Iterable[Struct]]) extends CostNode {
    
    // version with partial functions in the list
    def implementations: List[PartialFunction[
      (Rep[Iterable[Struct]], Rep[Iterable[Struct]], Targets), Rep[Iterable[Struct]]]] = List({
        case h @ HIndex(v1) => // hash join
          import IR._
          val result = new ArrayBuffer()
          while (v2.hasNext) {
            val val2 = v2.next
            if (h.contains(val2)) 
              result += (h.get(val2), val2)
          }
          result
          }, {
          case _ => // nested loop join
            import IR._
            val result = new ArrayBuffer()
            while(v2.hasNext) {
              val val1 = v1.next
              while(v1.hasNext) {
                val val2 = v2.next
                if(val1 == val2)
                  result += (val1, val2)
              }
           }
           result
        })
  }
}*/