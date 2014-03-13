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

  def setIndices(field : Field) {
    indexInner = fieldToIndexInner(field, dir);
    indexOuter = fieldToIndexOuter(field, dir);
    indexBorder = fieldToIndexBorder(field, dir);
    indexOpposingInner = fieldToIndexInner(field, dir.map(i => -i));
    indexOpposingOuter = fieldToIndexOuter(field, dir.map(i => -i));
    indexOpposingBorder = fieldToIndexBorder(field, dir.map(i => -i));
  }

  def setIndicesWide(field : Field) {
    indexInner = fieldToIndexInnerWide(field, dir);
    indexOuter = fieldToIndexOuterWide(field, dir);
    indexBorder = fieldToIndexBorder(field, dir);
    indexOpposingInner = fieldToIndexInnerWide(field, dir.map(i => -i));
    indexOpposingOuter = fieldToIndexOuterWide(field, dir.map(i => -i));
    indexOpposingBorder = fieldToIndexBorder(field, dir.map(i => -i));
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
  def numInnerPoints(level : Int, dim : Int) : Int = {
    if (dim < Knowledge.dimensionality)
      return (Knowledge.domain_fragLengthPerDim(dim) * (1 << level)) + 1;
    else
      return 1;
  }

  def resolveMultiIdx(field : Field, index : MultiIndex) : Expression = {
    return (index(2) * (field.layout(1).total * field.layout(0).total) + index(1) * field.layout(0).total + index(0));
  }
}

object fieldToIndexInner extends ((Field, Array[Int]) => IndexRange) {
  def apply(field : Field, dir : Array[Int]) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(field.level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(field.level, i) + Knowledge.data_numGhostLayers + 1)
          case i if dir(i) > 0  => (Mapping.last(field.level, i) - 2 * Knowledge.data_numGhostLayers)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(field.level, i) - Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(field.level, i) + 2 * Knowledge.data_numGhostLayers)
          case i if dir(i) > 0  => (Mapping.last(field.level, i) - Knowledge.data_numGhostLayers - 1)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}

object fieldToIndexInnerWide extends ((Field, Array[Int]) => IndexRange) {
  def apply(field : Field, dir : Array[Int]) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(field.level, i))
          case i if dir(i) < 0  => (Mapping.first(field.level, i) + Knowledge.data_numGhostLayers + 1)
          case i if dir(i) > 0  => (Mapping.last(field.level, i) - 2 * Knowledge.data_numGhostLayers)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(field.level, i))
          case i if dir(i) < 0  => (Mapping.first(field.level, i) + 2 * Knowledge.data_numGhostLayers)
          case i if dir(i) > 0  => (Mapping.last(field.level, i) - Knowledge.data_numGhostLayers - 1)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}

object fieldToIndexOuter extends ((Field, Array[Int]) => IndexRange) {
  def apply(field : Field, dir : Array[Int]) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(field.level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(field.level, i))
          case i if dir(i) > 0  => (Mapping.last(field.level, i) - Knowledge.data_numGhostLayers + 1)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(field.level, i) - Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(field.level, i) + Knowledge.data_numGhostLayers - 1)
          case i if dir(i) > 0  => (Mapping.last(field.level, i))
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}

object fieldToIndexOuterWide extends ((Field, Array[Int]) => IndexRange) {
  def apply(field : Field, dir : Array[Int]) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(field.level, i))
          case i if dir(i) < 0  => (Mapping.first(field.level, i))
          case i if dir(i) > 0  => (Mapping.last(field.level, i) - Knowledge.data_numGhostLayers + 1)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(field.level, i))
          case i if dir(i) < 0  => (Mapping.first(field.level, i) + Knowledge.data_numGhostLayers - 1)
          case i if dir(i) > 0  => (Mapping.last(field.level, i))
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}

object fieldToIndexBorder extends ((Field, Array[Int]) => IndexRange) {
  def apply(field : Field, dir : Array[Int]) : IndexRange = {
    return new IndexRange(
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.first(field.level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(field.level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) > 0  => (Mapping.last(field.level, i) - Knowledge.data_numGhostLayers)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)),
      new MultiIndex(
        (0 until Knowledge.dimensionality).toArray.map(i => i match {
          case i if dir(i) == 0 => (Mapping.last(field.level, i) - Knowledge.data_numGhostLayers)
          case i if dir(i) < 0  => (Mapping.first(field.level, i) + Knowledge.data_numGhostLayers)
          case i if dir(i) > 0  => (Mapping.last(field.level, i) - Knowledge.data_numGhostLayers)
        }) ++
          (Knowledge.dimensionality + 0 until 3).toArray.map(i => 0)));
  }
}
