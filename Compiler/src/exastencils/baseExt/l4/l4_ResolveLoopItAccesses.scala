package exastencils.baseExt.l4

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.datastructures.l4.UnresolvedAccess

/// L4_ResolveLoopItAccesses

object L4_ResolveLoopItAccesses extends DefaultStrategy("Resolve accesses to loop iterators") {
  this += new Transformation("Resolve iterator accesses", {
    // basic loop iterators
    case UnresolvedAccess("x", _, None, _, _, _) => L4_VariableAccess("x", L4_IntegerDatatype)
    case UnresolvedAccess("y", _, None, _, _, _) => L4_VariableAccess("y", L4_IntegerDatatype)
    case UnresolvedAccess("z", _, None, _, _, _) => L4_VariableAccess("z", L4_IntegerDatatype)

    // fragmentIdx
    case UnresolvedAccess("fragmentIdx", _, None, _, _, _) => L4_VariableAccess("fragmentIdx", L4_IntegerDatatype)

    // TODO: other potential iterators
  })
}
