package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_SpecialDatatype
import exastencils.base.ir.IR_VariableAccess

/// IR_WaLBerlaFieldIteratorAccess

object IR_WaLBerlaFieldIteratorAccess {
  def apply(dim : Int) = {
    val ret = new IR_WaLBerlaFieldIteratorAccess()
    ret.dim = dim
    ret
  }

  def fullIndex(numDims : Int) = IR_ExpressionIndex((0 until numDims).map(this (_) : IR_Expression).toArray)
}

class IR_WaLBerlaFieldIteratorAccess() extends IR_VariableAccess("i0", IR_SpecialDatatype("cell_idx_t")) {
  private var dim_ : Int = 0
  def dim_=(d : Int) = {
    dim_ = d
    name = s"i$dim_"
  }
  def dim = dim_

  override def equals(obj : scala.Any) = {
    obj match {
      case other : IR_WaLBerlaFieldIteratorAccess => other.dim == dim
      case other : IR_VariableAccess              => other.name == name
      case _                                      => super.equals(obj)
    }
  }
}

/*
// old stuff
object IR_WaLBerlaReplaceFieldIteratorAccesses extends DefaultStrategy("Replace field iterators in loops") {

  val regexIter : Regex = "\\d$".r // extract dim from iterator name, e.g. "i0" -> 0

  this += Transformation("Replace iterators", {
    case it : IR_FieldIteratorAccess =>
      IR_WaLBerlaFieldIteratorAccess(it.dim)
  })

  this += Transformation("Replace loop iterators", {
    case IR_ForLoop(begin, end, incr, bdy, parInfo) =>
      val decl = Duplicate(begin).asInstanceOf[IR_VariableDeclaration]
      val acc = IR_VariableAccess(decl.name, decl.datatype)
      val b = acc match {
        case iter : IR_FieldIteratorAccess               =>
          new IR_VariableDeclaration(IR_WaLBerlaFieldIteratorAccess(iter.dim).datatype, IR_WaLBerlaFieldIteratorAccess(iter.dim).name, decl.initialValue)
        case IR_VariableAccess(name, IR_IntegerDatatype) =>
          regexIter.findFirstMatchIn(name) match {
            case Some(m) =>
              new IR_VariableDeclaration(IR_WaLBerlaFieldIteratorAccess(m.group(0).toInt).datatype, IR_WaLBerlaFieldIteratorAccess(m.group(0).toInt).name, decl.initialValue)
            case None    =>
              Logger.dbg("No match")
              decl
          }
        case _                                           => begin
      }
      IR_ForLoop(b, end, incr, bdy, parInfo)

    /*
    case decl @ IR_VariableDeclaration(dt, name, _, _) =>
      IR_VariableAccess(name, dt) match {
        case iter : IR_FieldIteratorAccess               =>
          new IR_VariableDeclaration(IR_WaLBerlaFieldIteratorAccess(iter.dim).datatype, IR_WaLBerlaFieldIteratorAccess(iter.dim).name, decl.initialValue)
        case IR_VariableAccess(name, IR_IntegerDatatype) =>
          regexIter.findFirstMatchIn(name) match {
            case Some(m) =>
              new IR_VariableDeclaration(IR_WaLBerlaFieldIteratorAccess(m.group(0).toInt).datatype, IR_WaLBerlaFieldIteratorAccess(m.group(0).toInt).name, decl.initialValue)
            case None    =>
              Logger.dbg("No match")
              decl
          }
        case _                                           => decl
      }
     */
  })
}
*/
