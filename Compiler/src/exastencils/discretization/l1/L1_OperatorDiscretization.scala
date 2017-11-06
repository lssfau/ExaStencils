package exastencils.discretization.l1

import scala.collection.mutable.ListBuffer

import exastencils.base.l1._
import exastencils.operator.l1.L1_OperatorCollection
import exastencils.prettyprinting.PpStream

/// L1_OperatorDiscretization

case class L1_OperatorDiscretization(
    var src : String,
    var levels : Option[L1_DeclarationLevelSpecification],
    var mapping : Option[String],
    var discretization : L1_StringConstant) extends L1_DiscretizationStatement {

  override def prettyprint(out : PpStream) = {
    out << src
    if (mapping.isDefined) out << " => " << mapping.get
    out << " with " << discretization
  }

  override def process() = {
    val equations = {
      if (levels.isDefined) // explicit level definition is given -> extract levels and map to equations
        L1_LevelSpecification.extractLevelListDefEmpty(levels).to[ListBuffer].map(lvl => L1_OperatorCollection.getByIdentifier(src, lvl).get)
      else // otherwise collect all equations with the src name
        L1_OperatorCollection.getAllByIdentifier(src)
    }

    equations.foreach(l1Operator => {
      // TODO: discretization
      //val l2Operator = l1Operator.getProgressedObj()

      //if (mapping.isDefined) l2Operator.name = mapping.get
    })
  }
}
