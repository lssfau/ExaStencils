package exastencils.base.ir

import exastencils.baseExt.ir._
import exastencils.config.Platform
import exastencils.prettyprinting.PpStream

object IR_VariableDeclaration {
  def apply(datatype : IR_Datatype, name : String, initialValue : IR_Expression)
  = new IR_VariableDeclaration(datatype, name, Option(initialValue))
  def apply(variable : IR_VariableAccess)
  = new IR_VariableDeclaration(variable.datatype, variable.name, None)
  def apply(variable : IR_VariableAccess, initialValue : IR_Expression)
  = new IR_VariableDeclaration(variable.datatype, variable.name, Some(initialValue))
}

case class IR_VariableDeclaration(var datatype : IR_Datatype, var name : String, var initialValue : Option[IR_Expression] = None) extends IR_Statement {
  var alignment : Int = 1

  override def prettyprint(out : PpStream) : Unit = {
    // TODO: extract specialized behavior
    datatype match {
      case x : IR_VectorDatatype =>
        out << x << ' ' << name
        if (initialValue.isDefined) {
          out << "("
          initialValue.get match {
            case init : IR_VectorExpression => init.prettyprintInner(out)
            case sthElse                    => out << sthElse
          }
          out << ")"
        }

      case x : IR_MatrixDatatype =>
        if(exastencils.config.Knowledge.experimental_internalHighDimTypes) {
          x.datatype.prettyprint(out)
          out << ' ' << name << '[' << x.sizeM << ']' << '[' << x.sizeN << ']'
        } else {
          out << x << ' ' << name
          if (initialValue.isDefined) {
            out << "("
            initialValue.get match {
              case init : IR_MatrixExpression => init.prettyprintInner(out)
              case sthElse                    => out << sthElse
            }
            out << ")"
          }
        }

      case _ =>
        if (alignment > 1 && "MSVC" == Platform.targetCompiler)
          out << "__declspec(align(" << alignment * 8 << ")) "
        out << datatype.resolveDeclType << ' ' << name << datatype.resolveDeclPostscript
        if (alignment > 1 && "MSVC" != Platform.targetCompiler)
          out << " __attribute__((aligned(" << alignment * 8 << ")))"
        if (initialValue.isDefined)
          out << " = " << initialValue.get
    }

    out << ';'
  }

  /// prints only the declaration, ie omits (potential) initialization
  def prettyprintDeclaration() : String = IR_VariableDeclaration(datatype, name, None).prettyprint()
}
