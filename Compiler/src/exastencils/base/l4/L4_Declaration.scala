package exastencils.base.l4

import exastencils.base.ir._
import exastencils.baseExt.l4._
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4._

/// L4_ValueDeclaration

case class L4_ValueDeclaration(
    override var identifier : L4_Identifier,
    var datatype : L4_Datatype,
    var initialValue : L4_Expression) extends L4_Statement with L4_HasIdentifier {

  (datatype, initialValue) match { // FIXME does not work if initialValue is nested, e.g. inverse(L4_MatrixExpression)
    case (dt : L4_VectorDatatype, exp : L4_VectorExpression) => {
      initialValue.asInstanceOf[L4_VectorExpression].datatype = Some(dt.datatype)
      initialValue.asInstanceOf[L4_VectorExpression].convertConstants(dt.datatype)
      if (dt.isRow != exp.rowVector) Logger.error(s"Mismatch in assignment for Value ${ identifier.fullName }: incompatible vector types")
    }
    case (dt : L4_MatrixDatatype, exp : L4_MatrixExpression) => {
      initialValue.asInstanceOf[L4_MatrixExpression].datatype = Some(dt.datatype)
      initialValue.asInstanceOf[L4_MatrixExpression].convertConstants(dt.datatype)
    }
    case _                                                   =>
  }

  override def prettyprint(out : PpStream) = out << "Value " << identifier << " : " << datatype << " = " << initialValue

  override def progress : IR_VariableDeclaration = {
    Logger.warn(s"Progressing L4_ValueDeclaration with ${ identifier.fullName }")
    IR_VariableDeclaration(datatype.progress, identifier.fullName, initialValue.progress)
  }
}

/// L4_VariableDeclaration

case class L4_VariableDeclaration(
    override var identifier : L4_Identifier,
    var datatype : L4_Datatype,
    var initialValue : Option[L4_Expression] = None) extends L4_Statement with L4_HasIdentifier {

  override def prettyprint(out : PpStream) = {
    out << "Variable " << identifier << " : " << datatype
    if (initialValue.isDefined) out << " = " << initialValue.get
  }

  if (initialValue.isDefined) {
    (datatype, initialValue.get) match { // FIXME does not work if initialValue is nested, e.g. inverse(L4_MatrixExpression)
      case (dt : L4_VectorDatatype, exp : L4_VectorExpression) => {
        initialValue.get.asInstanceOf[L4_VectorExpression].datatype = Some(dt.datatype)
        initialValue.get.asInstanceOf[L4_VectorExpression].convertConstants(dt.datatype)
        if (dt.isRow != exp.rowVector) Logger.error(s"Mismatch in assignment for Variable ${ identifier.fullName }: incompatible vector types")
      }
      case (dt : L4_MatrixDatatype, exp : L4_MatrixExpression) => {
        initialValue.get.asInstanceOf[L4_MatrixExpression].datatype = Some(dt.datatype)
        initialValue.get.asInstanceOf[L4_MatrixExpression].convertConstants(dt.datatype)
      }
      case _                                                   =>
    }
  }

  override def progress = IR_VariableDeclaration(datatype.progress, identifier.fullName, L4_ProgressOption(initialValue)(_.progress))
}

/// L4_UnfoldLeveledDeclarations

object L4_UnfoldLeveledDeclarations extends DefaultStrategy("Unfold leveled declarations") {
  val levelCollector = new L4_LevelCollector
  register(levelCollector)

  def getLevelScope = if (levelCollector.inLevelScope) Some(levelCollector.getCurrentLevel) else None

  this += new Transformation("Unfold value and variable declarations", {
    case decl @ L4_ValueDeclaration(L4_LeveledIdentifier(_, levels), _, _)    => L4_Identifier.doDuplicate(decl, levels, getLevelScope)
    case decl @ L4_VariableDeclaration(L4_LeveledIdentifier(_, levels), _, _) => L4_Identifier.doDuplicate(decl, levels, getLevelScope)
  })
}

/// L4_InlineValueDeclarations

object L4_InlineValueDeclarations extends DefaultStrategy("Propagate and inline value declarations") {
  var valueCollector = new L4_ValueCollector
  register(valueCollector)

  // resolve values in expressions by replacing them with their expression => let SimplifyStrategy do the work
  this += new Transformation("Resolve values in expressions", {
    case x @ L4_UnresolvedAccess(_, None, None, _, None, _)                        =>
      val value = valueCollector.getValue(x.name)
      value match {
        case None => x // no hit
        case _    => Duplicate(value.get)
      }
    case x @ L4_UnresolvedAccess(_, Some(L4_SingleLevel(level)), None, _, _, None) =>
      val value = valueCollector.getValue(x.name + "@@" + level)
      value match {
        case None => x // no hit
        case _    => Duplicate(value.get)
      }
  })

  this += new Transformation("Remove propagated value declarations", {
    case globals : L4_GlobalSection => globals // skip global declarations
    case _ : L4_ValueDeclaration    => None
  }, false /* don't descend into globals */)
}
