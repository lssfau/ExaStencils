package exastencils.base.l4

import exastencils.base.ir._
import exastencils.baseExt.l4.L4_GlobalSection
import exastencils.core.Duplicate
import exastencils.core.collectors.L4ValueCollector
import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_ValueDeclaration

case class L4_ValueDeclaration(
    override var identifier : Identifier,
    var datatype : L4_Datatype,
    var initialValue : L4_Expression) extends L4_Statement with HasIdentifier {

  override def prettyprint(out : PpStream) = out << "Value " << identifier << " : " << datatype << " = " << initialValue << '\n'

  override def progress : IR_VariableDeclaration = {
    Logger.warn(s"Progressing L4_ValueDeclaration with ${ identifier.fullName }")
    IR_VariableDeclaration(datatype.progress, identifier.fullName, initialValue.progress)
  }
}

/// L4_VariableDeclaration

case class L4_VariableDeclaration(
    override var identifier : Identifier,
    var datatype : L4_Datatype,
    var initialValue : Option[L4_Expression] = None) extends L4_Statement with HasIdentifier {

  override def prettyprint(out : PpStream) = {
    out << "Variable " << identifier << " : " << datatype
    if (initialValue.isDefined) out << " = " << initialValue.get
    out << '\n'
  }

  override def progress = IR_VariableDeclaration(datatype.progress, identifier.fullName, L4_ProgressOption(initialValue)(_.progress))
}

/// L4_InlineValueDeclarations

object L4_InlineValueDeclarations extends DefaultStrategy("Propagate and inline value declarations") {
  var valueCollector = new L4ValueCollector
  register(valueCollector)

  // resolve values in expressions by replacing them with their expression => let SimplifyStrategy do the work
  this += new Transformation("Resolve values in expressions", {
    case x @ UnresolvedAccess(_, None, None, _, None, _)                                  =>
      val value = valueCollector.getValue(x.name)
      value match {
        case None => x // no hit
        case _    => Duplicate(value.get)
      }
    case x @ UnresolvedAccess(_, None, Some(SingleLevelSpecification(level)), _, None, _) =>
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
