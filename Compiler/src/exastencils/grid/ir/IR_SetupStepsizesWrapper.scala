package exastencils.grid.ir

import exastencils.config.Knowledge
import exastencils.domain.ir.IR_DomainCollection
import exastencils.domain.ir.IR_DomainFromAABB
import exastencils.field.ir.IR_FieldCollection
import exastencils.field.ir.IR_FieldLayoutPerDim
import exastencils.scheduling.NoStrategyWrapper

/// IR_SetupStepsizesWrapper

object IR_SetupStepsizesWrapper extends NoStrategyWrapper {
  override def callback : () => Unit = () => {
    // HACK: create discr_h* again if there are no multigrid level and the field size was defined explicitly
    //   currently this works only if all fields are equally sized
    if (Knowledge.domain_rect_generate && Knowledge.maxLevel <= 0) {
      def globalSize = IR_DomainCollection.getByIdentifier("global").get.asInstanceOf[IR_DomainFromAABB].aabb

      val fLayout : Array[IR_FieldLayoutPerDim] = IR_FieldCollection.objects.head.layout.layoutsPerDim
      Knowledge.discr_hx = Array[Double](globalSize.width(0) /
        (Knowledge.domain_rect_numFragsTotal_x * Knowledge.domain_fragmentLength_x * fLayout(0).numInnerLayers))
      if (Knowledge.dimensionality > 1)
        Knowledge.discr_hy = Array[Double](globalSize.width(1) /
          (Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_fragmentLength_y * fLayout(1).numInnerLayers))
      if (Knowledge.dimensionality > 2)
        Knowledge.discr_hz = Array[Double](globalSize.width(2) /
          (Knowledge.domain_rect_numFragsTotal_z * Knowledge.domain_fragmentLength_z * fLayout(2).numInnerLayers))
    }
  }
}
