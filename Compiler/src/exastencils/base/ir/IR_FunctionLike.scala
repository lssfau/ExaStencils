package exastencils.base.ir

trait IR_FunctionLike extends IR_Statement {
  def name : String
  def name_=(newName : String)

  def prettyprint_decl() : String

  var isHeaderOnly : Boolean = false
  var allowInlining : Boolean = true
  var allowFortranInterface : Boolean = true
  var functionQualifiers : String = "" // e.g. "__global__" etc
}
