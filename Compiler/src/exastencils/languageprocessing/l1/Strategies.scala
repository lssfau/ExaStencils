package exastencils.languageprocessing.l1

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.l1._
import exastencils.logger._

object UnifyOperators extends DefaultStrategy("Unification and simplification of mathematical operators") {
  def sanitize(s : String) = {
    s.replaceAll("\\", "__backslash__")
  }

  this += new Transformation("Search and destroy", {
    case BinaryExpression("*", Access("dx"), Access("dx")) => Access("dxx")
    case BinaryExpression("*", Access("dy"), Access("dy")) => Access("dyy")
    case BinaryExpression("*", Access("dz"), Access("dz")) => Access("dzz")
    case BinaryExpression("*", Access("dt"), Access("dt")) => Access("dtt")
    case Access("dx²") => Access("dxx")
    case Access("dy²") => Access("dyy")
    case Access("dz²") => Access("dzz")
    case Access("dt²") => Access("dtt")
    case BinaryExpression("^", Access("dx"), IntegerConstant(2)) => Access("dxx")
    case BinaryExpression("^", Access("dy"), IntegerConstant(2) | FloatConstant(2)) => Access("dyy")
    case BinaryExpression("^", Access("dz"), IntegerConstant(2) | FloatConstant(2)) => Access("dzz")
    case BinaryExpression("^", Access("dt"), IntegerConstant(2) | FloatConstant(2)) => Access("dtt")
  })
}
