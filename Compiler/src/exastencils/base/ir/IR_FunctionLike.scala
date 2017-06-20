package exastencils.base.ir

trait IR_FunctionLike extends IR_Statement {
  def name : String
  def fullName : String = name

  def prettyprint_decl() : String

  var isHeaderOnly : Boolean = false
}
