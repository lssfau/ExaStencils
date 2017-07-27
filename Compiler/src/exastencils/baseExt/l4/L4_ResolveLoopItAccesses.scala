package exastencils.baseExt.l4

import exastencils.base.l4._
import exastencils.datastructures._

/// L4_ResolveLoopItAccesses

object L4_ResolveLoopItAccesses extends DefaultStrategy("Resolve accesses to loop iterators") {
  this += new Transformation("Resolve iterator accesses", {
    // basic loop iterators
    case L4_UnresolvedAccess("x", _, None, _, _, _) => L4_PlainVariableAccess("x", L4_IntegerDatatype, false)
    case L4_UnresolvedAccess("y", _, None, _, _, _) => L4_PlainVariableAccess("y", L4_IntegerDatatype, false)
    case L4_UnresolvedAccess("z", _, None, _, _, _) => L4_PlainVariableAccess("z", L4_IntegerDatatype, false)

    // fragmentIdx
    case L4_UnresolvedAccess("fragmentIdx", _, None, _, _, _) => L4_PlainVariableAccess("fragmentIdx", L4_IntegerDatatype, false)

    // TODO: other potential iterators
  })
}
