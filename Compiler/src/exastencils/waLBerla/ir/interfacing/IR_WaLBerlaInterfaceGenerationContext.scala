package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.waLBerla.ir.field._

case class IR_WaLBerlaInterfaceGenerationContext(var members : ListBuffer[IR_WaLBerlaInterfaceMember]) {

  val uniqueWbFields = IR_WaLBerlaFieldCollection.objects.groupBy(_.name).map(_._2.head).to[ListBuffer] // find unique wb fields
    .sortBy(_.name)

  var publicMemberDeclarationMap : LinkedHashMap[String, IR_VariableDeclaration] = LinkedHashMap()
  var privateMemberDeclarationMap : LinkedHashMap[String, IR_VariableDeclaration] = LinkedHashMap()

  var memberCtorMap : LinkedHashMap[String, IR_Statement] = LinkedHashMap()
  var memberDtorMap : LinkedHashMap[String, IR_Statement] = LinkedHashMap()

  var ifaceParamMap : LinkedHashMap[String, IR_FunctionArgument] = LinkedHashMap()
  var ifaceInitializerListEntryMap : LinkedHashMap[String, (IR_Access, IR_Expression)] = LinkedHashMap()

  // get interface members and setup decls, ctors/dtors
  for (member <- members.sorted) {
    // register member decls, ctors and dtors
    if (member.isPrivate)
      member.registerIV(privateMemberDeclarationMap, memberCtorMap, memberDtorMap)
    else
      member.registerIV(publicMemberDeclarationMap, memberCtorMap, memberDtorMap)

    // get iface ctor params and corresponding initializer list entries
    member match {
      case ifaceParam : IR_WaLBerlaInterfaceParameter =>
        ifaceParamMap += (ifaceParam.resolveName() -> ifaceParam.ctorParameter)
        ifaceInitializerListEntryMap += (ifaceParam.resolveName() -> ifaceParam.initializerListEntry)
      case _                                          =>
    }
  }

  // call init functions responsible for setting up exa data structures
  val initFunctions : ListBuffer[IR_PlainInternalFunctionReference] = Duplicate(IR_WaLBerlaInitExaWrapperFunctions.functions)
    .map(f => IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype))

  // call init functions responsible for setting up coupling
  val couplingFunctions : ListBuffer[IR_PlainInternalFunctionReference] = Duplicate(IR_WaLBerlaInitCouplingWrapperFunctions.functions)
    .map(f => IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype))

  var ifaceConstructors : ListBuffer[IR_Constructor] = ListBuffer()
  var ifaceDestructors : ListBuffer[IR_Destructor] = ListBuffer()

  // ctor #1: empty parameter & initializer list, execute static convenience functions to obtain blockDataIDs and blockForest
  {
    // body
    val ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    // exa data initialization in ctor body
    initFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    // call ctors of collected members
    ctorBody ++= memberCtorMap.values

    // exa & waLBerla data structures initialized -> setup coupling
    couplingFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    ifaceConstructors += IR_Constructor(IR_WaLBerlaInterface.interfaceName, ListBuffer(), IR_MemberInitializerList(), ctorBody)
  }

  // ctor #2: user provides pointers to existing data structures
  {
    // params
    var ctorParams = ListBuffer[IR_FunctionArgument]()
    ctorParams ++= ifaceParamMap.values

    // initializer list
    var ctorInitializerList = IR_MemberInitializerList()
    ctorInitializerList.arguments ++= ifaceInitializerListEntryMap.values

    // body
    val ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    // initialization in ctor body
    initFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    // call ctors of collected members (except those in iface ctor initializer list)
    ctorBody ++= memberCtorMap.filter { case (name, _) => !ifaceParamMap.keys.exists(_ == name) }.values

    // exa & waLBerla data structures initialized -> setup coupling
    couplingFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    ifaceConstructors += IR_Constructor(IR_WaLBerlaInterface.interfaceName, ctorParams, ctorInitializerList, ctorBody)
  }

  // dtor body
  val dtorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

  IR_WaLBerlaDeInitWrapperFunctions.functions foreach (f => dtorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))
  IR_WaLBerlaDeInitExaWrapperFunctions.functions foreach (f => dtorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))

  var ifaceDestructor : IR_Destructor = IR_Destructor(IR_WaLBerlaInterface.interfaceName, dtorBody)
}

object IR_WaLBerlaAddInterfaceMembers extends QuietDefaultStrategy("Add waLBerla iface members") {
  var collectedMembers = ListBuffer[IR_WaLBerlaInterfaceMember]()
  this += Transformation("..", {
    case iv : IR_WaLBerlaInterfaceMember =>
      collectedMembers += iv
      iv
  })

  this += Transformation("..", {
    case wbColl : IR_WaLBerlaCollection =>
      wbColl.interfaceContext = Some(IR_WaLBerlaInterfaceGenerationContext(collectedMembers.sorted))
      wbColl
  })
}
