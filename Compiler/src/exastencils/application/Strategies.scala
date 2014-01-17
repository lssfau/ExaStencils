package exastencils.application

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.datastructures.Transformation._
import exastencils.application._
import exastencils.strategies._
import exastencils.primitives._

object SetupApplication extends Strategy("Setting up application") {
  val fieldCollection = FindFirstOccurence.find[FieldCollection].get;

  this += new Transformation("Setting up init fields", {
    case initFields : InitFields =>
      Some(initFields.expandSpecial(fieldCollection));
  });
}
