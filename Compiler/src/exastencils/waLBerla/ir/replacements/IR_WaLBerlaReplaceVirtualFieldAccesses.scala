package exastencils.waLBerla.ir.replacements

import exastencils.datastructures.Transformation
import exastencils.grid.ir._
import exastencils.waLBerla.ir.grid._

object IR_WaLBerlaReplaceVirtualFieldAccesses extends IR_WaLBerlaReplacementStrategy("Replace vf accesses with waLBerla function calls") {
  // replace vf accesses
  this += Transformation("Replace", {
    case _ @ IR_VirtualFieldAccess(IR_VF_CellCenterPerDim(lvl, domain, dim), idx, fragIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_VirtualFieldAccess(IR_WaLBerlaCellCenterPerDim(lvl, domain, dim), idx, fragIdx)
    case _ @ IR_VirtualFieldAccess(IR_VF_CellCenterAsVec(lvl, domain), idx, fragIdx) if inWaLBerlaBlockLoop(collector)       =>
      IR_VirtualFieldAccess(IR_WaLBerlaCellCenterAsVec(lvl, domain), idx, fragIdx)

    case _ @ IR_VirtualFieldAccess(IR_VF_CellWidthPerDim(lvl, domain, dim), idx, fragIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_VirtualFieldAccess(IR_WaLBerlaCellWidthPerDim(lvl, domain, dim), idx, fragIdx)
    case _ @ IR_VirtualFieldAccess(IR_VF_CellWidthAsVec(lvl, domain), idx, fragIdx) if inWaLBerlaBlockLoop(collector)       =>
      IR_VirtualFieldAccess(IR_WaLBerlaCellWidthAsVec(lvl, domain), idx, fragIdx)

    // boundary positions snap from cell center to node position -> introduce waLBerla node positions
    case _ @ IR_VirtualFieldAccess(IR_VF_NodePositionPerDim(lvl, domain, dim), idx, fragIdx) if inWaLBerlaBlockLoop(collector) =>
      IR_VirtualFieldAccess(IR_WaLBerlaNodePositionPerDim(lvl, domain, dim), idx, fragIdx)
    case _ @ IR_VirtualFieldAccess(IR_VF_NodePositionAsVec(lvl, domain), idx, fragIdx) if inWaLBerlaBlockLoop(collector)       =>
      IR_VirtualFieldAccess(IR_WaLBerlaNodePositionAsVec(lvl, domain), idx, fragIdx)
  })
}