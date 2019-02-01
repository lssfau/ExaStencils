package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.field.ir.IR_DirectFieldAccess
import exastencils.logger.Logger

case class IR_CommTransformation(var dim : Int, var id : Int) {
  //TODO extend to 3D-case

  // 90 degrees rotation
  private val rot90Mat = Vector(Vector(0,-1),Vector(1,0))
  // matMult only works for square matrices
  private def matMult (mat1 : Vector[Vector[Int]], mat2 : Vector[Vector[Int]]) = {
    val n = mat1.size

    var mat3 = Vector[Vector[Int]]()
    for(i <- 0 until n){
      var row = Vector[Int]()
      for(j <- 0 until n){
        var v : Int = 0
        for(k <- 0 until n){
          v += mat1(i)(k) * mat2(k)(j)
        }
        row = row :+ v
      }
      mat3 = mat3 :+ row
    }

    mat3
  }

  // create rotation matrix by applying 90 degree rotation several times
  private var rotMat = Vector(Vector(1,0),Vector(0,1))
  for(_ <- 0 until id)
    rotMat = matMult(rotMat, rot90Mat)

  private def transVec(fieldLevel: Int)= {
    def nGridPoints = (2 << fieldLevel) + 1
    var t = Vector[Int]()
    id match {
      case 0 => t = Vector(0,0)
      case 1 => t = Vector(nGridPoints,0)
      case 2 => t = Vector(nGridPoints,nGridPoints)
      case 3 => t = Vector(0,nGridPoints)
    }

    t
  }

  def applyTrafo(fieldAccess : IR_DirectFieldAccess) = {
    // Computation of new index:   newIndex = rotationMatrix * index + translationVector
    val index = fieldAccess.index
    val t = transVec(fieldAccess.fieldSelection.field.level)
    fieldAccess.index(0) = rotMat(0)(0) * index(0) + rotMat(0)(1) * index(1) + t(0)
    fieldAccess.index(1) = rotMat(1)(0) * index(0) + rotMat(1)(1) * index(1) + t(1)

    fieldAccess
  }
}

object IR_CommTransformationCollection{
  var trafos : ListBuffer[IR_CommTransformation] = ListBuffer[IR_CommTransformation]()

  def setup() = {
    trafos.clear()

    if(Knowledge.dimensionality == 2){
      // setup all trafos for 2D
      for(i <- 0 until 4)
        trafos = trafos :+ IR_CommTransformation(Knowledge.dimensionality, i)
    }
    else{
      Logger.error("IR_CommTransformationCollection cannot deal with dimensionality " + Knowledge.dimensionality)
    }
  }

}