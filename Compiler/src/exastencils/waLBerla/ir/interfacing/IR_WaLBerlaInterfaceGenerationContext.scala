package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.waLBerla.ir.field._

case class IR_WaLBerlaInterfaceGenerationContext(var functions : ListBuffer[IR_WaLBerlaFunction]) {

  val uniqueWbFields = IR_WaLBerlaFieldCollection.objects.groupBy(_.name).map(_._2.head).to[ListBuffer] // find unique wb fields
    .sortBy(_.name)

  var publicMemberDeclarationMap : LinkedHashMap[String, IR_VariableDeclaration] = LinkedHashMap()
  var privateMemberDeclarationMap : LinkedHashMap[String, IR_VariableDeclaration] = LinkedHashMap()

  var memberCtorMap : LinkedHashMap[String, IR_Statement] = LinkedHashMap()
  var memberDtorMap : LinkedHashMap[String, IR_Statement] = LinkedHashMap()

  var ifaceParamMap : LinkedHashMap[String, IR_FunctionArgument] = LinkedHashMap()
  var ifaceInitializerListEntryMap : LinkedHashMap[String, (IR_Access, IR_Expression)] = LinkedHashMap()

  // get interface members and setup decls, ctors/dtors
  CollectWaLBerlaInterfaceMembers.applyStandalone(functions)
  for (member <- CollectWaLBerlaInterfaceMembers.collectedMembers.sorted) {
    // get member declarations
    val decl = member.getDeclaration()
    if (member.isPrivate)
      privateMemberDeclarationMap += (member.name -> decl)
    else
      publicMemberDeclarationMap += (member.name -> decl)

    // get member ctors/dtors
    for (ctor <- member.getCtor())
      memberCtorMap += (member.name -> ctor)
    for (dtor <- member.getDtor())
      memberDtorMap += (member.name -> dtor)

    // get iface ctor params and corresponding initializer list entries
    member match {
      case ifaceParam : IR_WaLBerlaInterfaceParameter =>
        ifaceParamMap += (ifaceParam.name -> ifaceParam.ctorParameter)
        ifaceInitializerListEntryMap += (ifaceParam.name -> ifaceParam.initializerListEntry)
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

    // exa & waLBerla data structures initialized -> setup coupling
    couplingFunctions foreach (f => ctorBody += IR_FunctionCall(f))

    ifaceConstructors += IR_Constructor(IR_WaLBerlaInterface.interfaceName, ctorParams, ctorInitializerList, ctorBody)
  }

  // dtor body
  {
    val dtorBody : ListBuffer[IR_Statement] = ListBuffer(IR_NullStatement)

    IR_WaLBerlaDeInitWrapperFunctions.functions foreach (f => dtorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))
    IR_WaLBerlaDeInitExaWrapperFunctions.functions foreach (f => dtorBody += IR_FunctionCall(IR_PlainInternalFunctionReference(f.name, IR_UnitDatatype)))

    ifaceDestructors += IR_Destructor(IR_WaLBerlaInterface.interfaceName, dtorBody)
  }
}
