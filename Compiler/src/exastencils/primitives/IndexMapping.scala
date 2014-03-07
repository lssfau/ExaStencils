package exastencils.primitives

import exastencils.knowledge._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

class NeighborInfo(var dir : Array[Int], var index : Int) {
  var label : String = (Knowledge.dimensionality - 1 to 0 by -1).toList.map(i => dimToString(i).toUpperCase + dirToString(dir(i))).mkString("_");

  var indexInner = new IndexRange();
  var indexOuter = new IndexRange();
  var indexBorder = new IndexRange();

  var indexOpposingInner = new IndexRange();
  var indexOpposingOuter = new IndexRange();
  var indexOpposingBorder = new IndexRange();

  def setIndices(field : Field, level : Int) {
    indexInner = fieldToIndexInner(dir, level);
    indexOuter = fieldToIndexOuter(dir, level);
    indexBorder = fieldToIndexBorder(dir, level);
    indexOpposingInner = fieldToIndexInner(dir.map(i => -i), level);
    indexOpposingOuter = fieldToIndexOuter(dir.map(i => -i), level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), level);
  }

  def setIndicesWide(field : Field, level : Int) {
    indexInner = fieldToIndexInnerWide(dir, level);
    indexOuter = fieldToIndexOuterWide(dir, level);
    indexBorder = fieldToIndexBorder(dir, level);
    indexOpposingInner = fieldToIndexInnerWide(dir.map(i => -i), level);
    indexOpposingOuter = fieldToIndexOuterWide(dir.map(i => -i), level);
    indexOpposingBorder = fieldToIndexBorder(dir.map(i => -i), level);
  }
}

case class IndexRange(begin : MultiIndex = new MultiIndex, end : MultiIndex = new MultiIndex) {}

object Mapping {
  def first(level : Int, dim : Int) : Int = {
    return 0;
  }
  def last(level : Int, dim : Int) : Int = {
    return numPoints(level, dim) - 1;
  }
  def numPoints(level : Int, dim : Int) : Int = {
    if (dim < Knowledge.dimensionality)
      return (Knowledge.domain_fragLengthPerDim(dim) * (1 << level)) + 1 + 2 * Knowledge.data_numGhostLayers;
    else
      return 1;
  }
  def access(level : Int, x : Expression = "x", y : Expression = "y", z : Expression = "z") : Expression = {
    var exp : Expression = x;
    if (Knowledge.dimensionality > 1) exp += y * numPoints(level, 0);
    if (Knowledge.dimensionality > 2) exp += z * numPoints(level, 1) * numPoints(level, 0);
    return exp;
  }
  def access(level : Int, index : MultiIndex) : Expression = {
    return (index(2) * numPoints(level, 1) * numPoints(level, 0) + index(1) * numPoints(level, 0) + index(0));
  }
}

object fieldToIndexInner extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.data_numGhostLayers + 1)
          case i if dir(i) > 0  => (Mapping.last(level, i) - 2 * Knowledge.data_numGhostLayers)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(level, i) - Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(level, i) + 2 * Knowledge.data_numGhostLayers)
          case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.data_numGhostLayers - 1)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}

object fieldToIndexInnerWide extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(level, i))
          case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.data_numGhostLayers + 1)
          case i if dir(i) > 0  => (Mapping.last(level, i) - 2 * Knowledge.data_numGhostLayers)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(level, i))
          case i if dir(i) < 0  => (Mapping.first(level, i) + 2 * Knowledge.data_numGhostLayers)
          case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.data_numGhostLayers - 1)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}

object fieldToIndexOuter extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(level, i))
          case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.data_numGhostLayers + 1)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(level, i) - Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.data_numGhostLayers - 1)
          case i if dir(i) > 0  => (Mapping.last(level, i))
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}

object fieldToIndexOuterWide extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(level, i))
          case i if dir(i) < 0  => (Mapping.first(level, i))
          case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.data_numGhostLayers + 1)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(level, i))
          case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.data_numGhostLayers - 1)
          case i if dir(i) > 0  => (Mapping.last(level, i))
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}

object fieldToIndexBorder extends ((Array[Int], Int) => IndexRange) {
  def apply(dir : Array[Int], level : Int) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.data_numGhostLayers)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(level, i) - Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) > 0  => (Mapping.last(level, i) - Knowledge.data_numGhostLayers)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}
