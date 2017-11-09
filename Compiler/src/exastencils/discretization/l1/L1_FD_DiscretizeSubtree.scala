package exastencils.discretization.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1.L1_ImplicitConversion._
import exastencils.base.l1._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.l1._
import exastencils.operator.l1._

/// L1_FD_DiscretizeSubtree

object L1_FD_DiscretizeSubtree extends QuietDefaultStrategy("Discretize expressions recursively") {
  var domain : L1_Domain = _
  var level : Int = 0

  def numDims = domain.numDims

  this += new Transformation("Resolve Laplace operators", {
    case L1_Laplace => (0 until numDims).map(dim => L1_PartialDerivative(2, dim) : L1_Expression).reduce(_ + _)
  })

  this += new Transformation("Search and replace", {
    case L1_PartialDerivative(derOrder, dim) =>
      val domainExtends = domain.asInstanceOf[L1_DomainFromAABB].aabb
      val gridWidth = domainExtends.width(dim) / (Knowledge.domain_rect_numFragsTotalAsVec(dim) * Knowledge.domain_fragmentLengthAsVec(dim) * (1 << level))
      val errOrder = Knowledge.discr_fd_order

      val n = if ((derOrder + errOrder) % 2 == 1) derOrder + errOrder else derOrder + errOrder - 1
      val weights = Knowledge.discr_fd_scheme.toLowerCase() match {
        case "taylor"   => new L1_FD_TaylorApproach(n, derOrder, gridWidth, errOrder, 0).getWeights
        case "lagrange" => new L1_FD_LagrangeApproach(n, derOrder, gridWidth, 0).getWeights
      }

      val tmpStencil = L1_Stencil(s"partDer_${ derOrder }_$dim", level, numDims, ListBuffer())

      val reach = (n - 1) / 2
      for (offsetInDim <- -reach to reach) {
        val offset = L1_ConstIndex(Array.fill(numDims)(0))
        offset(dim) = offsetInDim
        tmpStencil.entries += L1_StencilEntry(offset, weights(offsetInDim + reach))
      }

      L1_StencilAccess(tmpStencil)

    case c : L1_Number =>
      val tmpStencil = L1_Stencil("tmp_const", level, numDims, ListBuffer(L1_StencilEntry(L1_ConstIndex(Array.fill(numDims)(0)), c)))
      L1_StencilAccess(tmpStencil)
  })
}
