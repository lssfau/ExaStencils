package exastencils.util

import exastencils.core.Duplicate

object NoDuplicateWrapper {
  Duplicate.dontClone(classOf[NoDuplicateWrapper[_]])
}

case class NoDuplicateWrapper[T](var value : T) {
  NoDuplicateWrapper // initialize object
}
