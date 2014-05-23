package exastencils.spl

trait MathematicalFunction {
  
  def computeValue(constants: Array[Double], value : Double) : Double
  
}

case object LinearFunction extends MathematicalFunction {  
    def computeValue(constants: Array[Double], value : Double) : Double = {
      return constants(0) * value + constants(1)
    }
}

case object QuadraticFunction extends MathematicalFunction {
    def computeValue(constants: Array[Double], value : Double) : Double = {
      return constants(0) * (value * value ) + constants(1) * value + constants(2)
    }
}

