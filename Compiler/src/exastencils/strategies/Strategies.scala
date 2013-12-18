package exastencils.strategies

import exastencils.core._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.domain._
import exastencils.primitives.OMP_PotentiallyCritical
import exastencils.primitives.Class // FIXME

object PrintStrategy extends Strategy("Pretty-Print") {
  this += new Transformation("Pretty-Print", {
    case printable : FilePrettyPrintable =>
      printable.printToFile;
      Some(printable);
  });
}

object ExpandStrategy extends Strategy("Expanding") {
  this += new Transformation("Hoho, expanding all day...", {
    case function : Expandable =>
      Some(function.expand);
  });
}

object AddMemberFunctionPrefixStrategy extends Strategy("Addin member function prefixes") {
  // FIXME: requires nested strategies which currently are not available
  //    this += new Transformation("Add function scope prefixes to class member functions", {
  //      case c : Class =>
  //        var strategyAddScopePrefix = new Strategy("strategyAddScopePrefix");
  //        strategyAddScopePrefix += new Transformation({
  //          case function : FunctionStatement =>
  //
  //            function.name = s"${c.className}::${f.name}";
  //
  //            Some(function);
  //        }, true, c)
  //
  //        strategyAddScopePrefix.apply;
  //
  //        Some(c);
  //    });
  this += new Transformation("Adding function scope prefixes to class member functions", {
    case c : Class =>
      for (func <- c.functions) {
        func match { case f : FunctionStatement => f.name = s"${c.className}::${f.name}"; }
      }
      Some(c);
  });
}

object AddOMPStrategy extends Strategy("Adding OMP pragmas") {
  // FIXME: currently this strategy is only applied to a single node per run
  this += new Transformation("Adding OMP pragmas", {
    case target : OMP_PotentiallyCritical =>
      Some(target.addOMPDirective);
  }/*FIXME: , false*/);
}