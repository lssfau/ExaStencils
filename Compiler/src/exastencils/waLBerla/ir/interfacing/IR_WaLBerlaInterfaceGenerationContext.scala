package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field._

case class IR_WaLBerlaInterfaceGenerationContext(var members : ListBuffer[IR_WaLBerlaInterfaceMember]) {

  val blockForest : IR_WaLBerlaBlockForest = IR_WaLBerlaBlockForest()

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

  // add variables from WaLBerlaVars sections as public members
  publicMemberDeclarationMap ++= IR_WaLBerlaCollection.get.variables.map(decl => decl.name -> decl)

  // call init functions responsible for setting up exa data structures
  val initExaFunctions : ListBuffer[IR_PlainInternalFunctionReference] = Duplicate(IR_WaLBerlaInitExaWrapperFunctions.functions)
    .map(f => IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype))

  val deInitExaFunctions : ListBuffer[IR_PlainInternalFunctionReference] = Duplicate(IR_WaLBerlaDeInitExaWrapperFunctions.functions)
    .map(f => IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype))

  // call init functions responsible for setting up coupling
  val couplingFunctions : ListBuffer[IR_PlainInternalFunctionReference] = Duplicate(IR_WaLBerlaInitCouplingWrapperFunctions.functions)
    .map(f => IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype))

  def initIVs(funcName : String, funcBody : Iterable[IR_Statement]) : IR_Statement = {
    val setupIVs = IR_WaLBerlaInitInterfaceVariables(funcName,
      funcBody.to[ListBuffer])
    IR_WaLBerlaCollection.get.functions += setupIVs

    IR_FunctionCall(setupIVs.name)
  }

  var ifaceConstructors : ListBuffer[IR_Constructor] = ListBuffer()

  // ctor #1: empty parameter & initializer list, execute static convenience functions to obtain blockDataIDs and blockForest
  {
    // body
    val ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    // exa data initialization in ctor body
    initExaFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    // call ctors of collected interface members
    ctorBody += initIVs("setupInterfaceVariables", memberCtorMap.values)

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
    initExaFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    // call ctors of collected members (except those in iface ctor initializer list)
    ctorBody += initIVs("setupInterfaceVariablesWithoutCtorParameters",
      memberCtorMap.filter { case (name, _) => !ifaceParamMap.keys.exists(_ == name) }.values)

    // exa & waLBerla data structures initialized -> setup coupling
    couplingFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    ifaceConstructors += IR_Constructor(IR_WaLBerlaInterface.interfaceName, ctorParams, ctorInitializerList, ctorBody)
  }

  // ctor #3: user provides pointer to existing blockforest (only generated if paramMap not only contains blockforest)
  private val blockForestIsOnlyCtorParam = ifaceParamMap.size == 1 && ifaceParamMap.head._2 == blockForest.ctorParameter
  if (!blockForestIsOnlyCtorParam) { // prevent duplicate ctors
    // params
    var ctorParams = ListBuffer[IR_FunctionArgument]()
    ctorParams += blockForest.ctorParameter

    // initializer list
    var ctorInitializerList = IR_MemberInitializerList()
    ctorInitializerList.arguments += blockForest.initializerListEntry

    // body
    val ctorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    // initialization in ctor body
    initExaFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    // call ctors of collected members (except the one for the blockforest)
    ctorBody += initIVs("setupInterfaceVariablesWithoutBlockforest",
      memberCtorMap.filter { case (name, _) => name != blockForest.resolveName() }.values)

    // exa & waLBerla data structures initialized -> setup coupling
    couplingFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    ifaceConstructors += IR_Constructor(IR_WaLBerlaInterface.interfaceName, ctorParams, ctorInitializerList, ctorBody)
  }

  // dtor
  val dtorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

  {
    deInitExaFunctions foreach (f => dtorBody += IR_FunctionCall(f))

    // add IV dtors to interface function and simply call in body
    val cleanupIVs = IR_WaLBerlaDeInitInterfaceVariables(memberDtorMap.values.to[ListBuffer])
    IR_WaLBerlaCollection.get.functions += cleanupIVs
    dtorBody += IR_FunctionCall(cleanupIVs.name)
  }

  var ifaceDestructor : IR_Destructor = IR_Destructor(IR_WaLBerlaInterface.interfaceName, dtorBody)
}

object IR_WaLBerlaAddInterfaceMembers extends DefaultStrategy("Add waLBerla iface members") {
  var collectedMembers = ListBuffer[IR_WaLBerlaInterfaceMember]()

  this += Transformation("Collect IV members", {
    case iv : IR_WaLBerlaInterfaceMember =>
      collectedMembers += iv
      iv
  })

  this += Transformation("Create interface context", {
    case wbColl : IR_WaLBerlaCollection =>
      wbColl.interfaceContext = Some(IR_WaLBerlaInterfaceGenerationContext(collectedMembers.sorted))
      wbColl
  })
}
