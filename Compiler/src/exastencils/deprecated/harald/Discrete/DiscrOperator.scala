package exastencils.deprecated.harald.Discrete

case class DiscrOperatorType(inputfunctype: DiscrFunctionType, outputfunctype: DiscrFunctionType)  extends DiscrType {
  override def toString = s"(${inputfunctype}) -> (${outputfunctype})"
}

case class DiscrOperator(nam : String, optype: DiscrOperatorType) extends DiscrObject {
  name = nam
  override def toString = s"${name} : ${optype}"

}