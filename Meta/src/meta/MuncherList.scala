package meta

import scala.collection.mutable.ListBuffer

/// MuncherList

object MuncherList {

  case class Entry(var filePackage : String, var className : String, var layers : ListBuffer[Layer])

  val entries = ListBuffer[Entry]()

  def all = ListBuffer(L2, L3, L4, IR)
  def L2_L3 = ListBuffer(L2, L3)
  def L2_L3_L4 = ListBuffer(L2, L3, L4)

  // in exastencils.base

  var curPackage = "exastencils.base"
  //entries += Entry(curPackage, "Access", all)
  entries += Entry(curPackage, "BinaryOp", L2_L3_L4)
  entries += Entry(curPackage, "Constant", L2_L3_L4)
  //entries += Entry(curPackage, "Datatype", all)
  entries += Entry(curPackage, "Expression", all)
  //entries += Entry(curPackage, "Function", all)
  entries += Entry(curPackage, "ImplicitConversion", all)
  //entries += Entry(curPackage, "Index", all)
  entries += Entry(curPackage, "LevelAlias", L2_L3_L4)
  entries += Entry(curPackage, "LevelCollector", L2_L3_L4)
  entries += Entry(curPackage, "LevelGroup", L2_L3_L4)
  entries += Entry(curPackage, "LevelSingle", L2_L3_L4)
  entries += Entry(curPackage, "LevelSpecification", L2_L3_L4)
  entries += Entry(curPackage, "Node", all)
  entries += Entry(curPackage, "Progressable", L2_L3_L4)
  entries += Entry(curPackage, "Root", L2_L3)
  entries += Entry(curPackage, "Statement", all)
  entries += Entry(curPackage, "UnaryOp", L2_L3_L4)

}
