package exastencils.waLBerla.l4

import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4.L4_FieldAccess
import exastencils.field.l4.L4_FutureFieldAccess

/// L4_WaLBerlaResolveFieldAccesses

object L4_WaLBerlaResolveFieldAccesses extends DefaultStrategy("Resolve accesses to waLBerla fields") {
  this += Transformation("Resolve", {
    case fAcc : L4_FutureFieldAccess if L4_WaLBerlaFieldCollection.contains(fAcc) =>
      val wbField = L4_WaLBerlaFieldCollection.getByFieldAccess(fAcc).get // get field from wb field collection

      L4_FieldAccess(wbField.field, fAcc.slot, fAcc.offset, fAcc.arrayIndex, fAcc.frozen, fAcc.matIndex) // create 'regular' access for it
  })
}
