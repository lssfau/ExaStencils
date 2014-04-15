import scala.collection.mutable.ListBuffer
import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.strategies._
import exastencils.application._
import exastencils.domain._
import exastencils.multiGrid._
import exastencils.primitives._
import exastencils.util._
import exastencils.globals._
import exastencils.prettyprinting.PrettyprintingManager
import harald.Parser._
import harald.dsl._
import harald.Generate._
import harald.ast._
import exastencils.spl.FeatureModel
import exastencils.parsers.l4.ParserL4
import exastencils.datastructures.l4.ProgressableToIr
import exastencils.languageprocessing.l4.ProgressToIr

object Main {
  def main(args : Array[String]) : Unit = {
    // Init settings

    if (args.length >= 1) {
      val s = new exastencils.parsers.settings.ParserSettings
      s.parseFile(args(0))
    }
    if (args.length >= 2) {
      val k = new exastencils.parsers.settings.ParserKnowledge
      k.parseFile(args(1))
    }

    // feature model  (modified FAMA Format (http://www.isa.us.es/fama/))
    FeatureModel.readFeatureModel("./Compiler/featureModel/model_Prototype.model")
    var configuration = FeatureModel.getMinimalConfig
    Knowledge.update(configuration)

    // Hack paths (relative paths should work here, too, if not, reverse this change)
    // ... this obviously depends on the execution path which in my case is the root folder to include configs and scripts
    val libpath = "./Compiler/src/harald/otherfiles/"
    val DSLpath = "./Compiler/src/harald/testmg/"
    val problem = "testDSL"
    val outputfile = "main.cpp"

    // HACK: this tests the new L4 capabilities
    var parserl4 = new ParserL4
    StateManager.root_ = parserl4.parseFile("./Compiler/src/harald/testmg/newDSL4.exa")
    ProgressToIr.apply

    StateManager.root_ = StateManager.root_.asInstanceOf[ProgressableToIr].progressToIr.asInstanceOf[Node]

    // Setup tree
    StateManager.root_.asInstanceOf[Root].nodes ++= List(
      // Application
      new Poisson3D,

      // Domain
      new DomainGenerated,

      // Primitives
      new FragmentClass,
      new CommunicationFunctions,

      // Util
      new Log,
      new Stopwatch,
      new Vector,

      // Globals
      new Globals)

    // FIXME: set fields in L4
    /*var fieldCollection = StateManager.findFirst[FieldCollection]().get
    for (level <- 0 to Knowledge.maxLevel) {
      { // fields requiring (ghost-layer) communication
        val layout = DimArray().map(dim => new FieldLayoutPerDim(if (0 == dim) 1 else 0, Knowledge.data_numGhostLayers, 1, ((Knowledge.domain_fragLengthPerDim(dim) * (1 << level)) + 1) - 2 /*dup*/ , 1, Knowledge.data_numGhostLayers, 0)) ++
          (Knowledge.dimensionality until 3).toArray.map(dim => new FieldLayoutPerDim(0, 0, 0, 1, 0, 0, 0))
        fieldCollection.fields += new Field("Solution", 0, "solData", "double", layout, level, Knowledge.data_numSolSlots, new MultiIndex(layout.map(l => l.idxDupLeftBegin)), true);
        fieldCollection.fields += new Field("Residual", 0, "resData", "double", layout, level, Knowledge.data_numSolSlots, new MultiIndex(layout.map(l => l.idxDupLeftBegin)), false);
      }
      { // fields without ghost layers
        val layout = DimArray().map(dim => new FieldLayoutPerDim(0, 0, 1, ((Knowledge.domain_fragLengthPerDim(dim) * (1 << level)) + 1) - 2 /*dup*/ , 1, 0, 0)) ++
          (Knowledge.dimensionality until 3).toArray.map(dim => new FieldLayoutPerDim(0, 0, 0, 1, 0, 0, 0))
        fieldCollection.fields += new Field("RHS", 0, "rhsData", "double", layout, level, Knowledge.data_numSolSlots, new MultiIndex(layout.map(l => l.idxDupLeftBegin)), false);
      }
    }*/

    // setup basic sub-nodes

    //do { ExpandStrategy.apply; }
    //while (ExpandStrategy.results.last._2.replacements > 0) // FIXME: cleaner code

    // Harald

    println("read HW")

    if (!new java.io.File(DSLpath + problem + "levHW.mg").exists) {
      println("HW specification is missing!")
      sys.exit(0)
    }

    val DSLHW : String = scala.io.Source.fromFile(DSLpath + problem + "levHW.mg").getLines.reduceLeft(_ + '\n' + _)
    //println(DSLHW)

    val parserHW = new ParserHW
    parserHW.parseAll(parserHW.exastencilsHW, DSLHW)

    harald.dsl /*FIXME*/ .Hardware.initHWFeatures

    if (!new java.io.File(DSLpath + problem + "lev1.mg").exists) {
      println("Problem specification (DSL level 1) is missing!")
      sys.exit(0)
    }

    println("read PDE")
    val DSLl1 : String = scala.io.Source.fromFile(DSLpath + problem + "lev1.mg").getLines.reduceLeft(_ + '\n' + _)
    //println(DSLl1)

    val parserl1 = new ParserL1
    parserl1.parseAll(parserl1.exastencilsL1, DSLl1)

    if (!new java.io.File(DSLpath + problem + "lev2.mg").exists || DomainKnowledge.generate_L1.getOrElse(1) == 1)
      GenerateL2.transformL1toL2(DSLpath + problem + "lev2.mg")

    println("read discretization")
    val DSLl2 : String = scala.io.Source.fromFile(DSLpath + problem + "lev2.mg").getLines.reduceLeft(_ + _)
    //println(DSLl2)

    val parserl2 = new ParserL2(TreeManager.tree)
    parserl2.parseAll(parserl2.exastencilsL2, DSLl2)
    TreeManager.tree.exaFields.foreach(println)
    TreeManager.tree.exaOperators.foreach(println)
    DomainKnowledge.initfragments

    val genL3 = new GenerateL3(TreeManager.tree)
    if (!new java.io.File(DSLpath + problem + "lev3.mg").exists || DomainKnowledge.generate_L1.getOrElse(1) == 1)
      genL3.transformL2toL3(DSLpath + problem + "lev3.mg")

    val DSLl3 : String = scala.io.Source.fromFile(DSLpath + problem + "lev3.mg").getLines.reduceLeft(_ + _)
    //println(DSLl3)

    val parserl3 = new ParserL3
    parserl3.parseAll(parserl3.exastencilsL3, DSLl3)
    DomainKnowledge.initarraysizes

    val genL4 = new GenerateL4(TreeManager.tree)
    if (!(new java.io.File(DSLpath + problem + "lev4.mg").exists) || (DomainKnowledge.generate_L1.getOrElse(1) == 1)) {
      genL4.transformL3toL4(DSLpath + problem + "lev4.mg")
      println("generate L4: " + DomainKnowledge.generate_L1.getOrElse(1))
    }
    val DSLl4 : String = scala.io.Source.fromFile(DSLpath + problem + "lev4.mg").getLines.reduceLeft(_ + _)
    //println(DSLl4)

    //    val parserl4_dep = new harald.Parser.ParserL4(TreeManager.tree)
    //    parserl4_dep.parse(DSLl4)

    // Strategies

    (new Strategy("FindStencilConvolutions") {
      this += new Transformation("SearchAndMark", {
        case BinaryExpression(BinaryOperators.Multiplication, UnresolvedStencilAccess(stencilName, stencilLevel), UnresolvedFieldAccess(fieldOwner, fieldName, fieldLevel, fieldSlot, fieldIndex)) =>
          StencilConvolution(StateManager.findFirst[StencilCollection]().get.getStencilByIdentifier(stencilName, stencilLevel).get,
            StateManager.findFirst[FieldCollection]().get.getFieldByIdentifier(fieldName, fieldLevel).get)
      })
    }).apply

    (new Strategy("ResolveSpecialFunctions") {
      this += new Transformation("SearchAndReplace", {
        case FunctionCallExpression(StringConstant("diag"), args) =>
          StateManager.findFirst[StencilCollection]().get.getStencilByIdentifier(
            args(0).asInstanceOf[UnresolvedStencilAccess].stencilIdentifier,
            args(0).asInstanceOf[UnresolvedStencilAccess].level).get.entries(0).weight

        // HACK to realize intergrid operations
        case FunctionCallExpression(StringConstant("ToCoarser"), args) =>
          var stencilConvolution = Duplicate(args(0).asInstanceOf[StencilConvolution])
          stencilConvolution.targetIdx = new MultiIndex(DimArray().map(i => (2 * (dimToString(i) : Expression)) : Expression))
          stencilConvolution
        case FunctionCallExpression(StringConstant("ToFiner"), args) =>
          var stencilConvolution = Duplicate(args(0).asInstanceOf[StencilConvolution])
          stencilConvolution.targetIdx = new MultiIndex(DimArray().map(i => ((dimToString(i) : Expression) / 2) : Expression))
          stencilConvolution

        // HACK to realize print function -> FIXME
        case ExpressionStatement(FunctionCallExpression(StringConstant("print"), args)) =>
          new Scope(ListBuffer[Statement](
            "int rank;",
            "MPI_Comm_rank(MPI_COMM_WORLD, &rank);",
            new ConditionStatement("0 == rank",
              ("std::cout << " : Expression) ~ args.reduceLeft((l, e) => l ~ "<< \" \" <<" ~ e) ~ "<< std::endl;")))

        // HACK to realize return functionality -> FIXME: move to specialized node
        case ExpressionStatement(FunctionCallExpression(StringConstant("return"), args)) =>
          args.size match {
            case 0 => "return;" : Statement
            case 1 => ("return " ~ args(0) ~ ";") : Statement
            case _ => "ERROR - unsupported return function statement" : Statement
          }
      })
    }).apply

    do { ExpandStrategy.apply; }
    while (ExpandStrategy.results.last._2.replacements > 0) // FIXME: cleaner code

    SetupFragmentClass.apply;
    SetupMultiGrid.apply;
    SetupApplication.apply;

    do { ExpandStrategy.apply; }
    while (ExpandStrategy.results.last._2.replacements > 0) // FIXME: cleaner code

    do { SimplifyStrategy.apply; }
    while (SimplifyStrategy.results.last._2.replacements > 0) // FIXME: cleaner code

    AddMemberFunctionPrefix.apply;

    if (Knowledge.useOMP) {
      AddOMPPragmas.apply;
    }

    PrintStrategy.apply;
    PrettyprintingManager.finish;

    println("Done!");
  }
}
