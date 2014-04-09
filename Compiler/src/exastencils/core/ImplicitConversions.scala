package exastencils.core

import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

object ImplicitConversions {
  implicit def LBtoL[T](lb : ListBuffer[T]) : List[T] = { lb.toList }
  implicit def LtoLB[T](l : List[T]) : ListBuffer[T] = { var x = ListBuffer[T](); x ++= l; x }
}