package exastencils.visualization.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverPoints
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess
import exastencils.hack.ir._
import exastencils.logger.Logger

/// IR_ResolveCImgFunctions

object IR_ResolveCImgFunctions extends DefaultStrategy("ResolveCImgFunctions") {
  this += new Transformation("ResolveFunctionCalls", {
    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("readImage", _), args)) =>
      if (args.size != 2 || !args.head.isInstanceOf[IR_FieldAccess]) {
        Logger.warn("Malformed call to readImage; usage: readImage ( field, \"filename\" )")
        IR_NullStatement
      } else {
        val field = args.head.asInstanceOf[IR_FieldAccess]
        val filename = args(1).asInstanceOf[IR_StringConstant].value

        val stmts = ListBuffer[IR_Statement]()

        //stmts += IR_FunctionCall(IR_UserFunctionReference("cimg_library::CImg< double > imageIn")
        stmts += HACK_IR_Native("cimg_library::CImg< double > imageIn ( \"" + filename + "\" )")
        // flip image for correct representation
        stmts += IR_MemberFunctionCall("imageIn", "mirror", HACK_IR_Native("'y'"))

        stmts += IR_LoopOverPoints(field.field,
          IR_Assignment(field, HACK_IR_Native("*imageIn.data(i0,i1)")))

        IR_Scope(stmts)
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("writeImage", _), args)) =>
      if (args.size != 2 || !args.head.isInstanceOf[IR_FieldAccess]) {
        Logger.warn("Malformed call to writeImage; usage: writeImage ( field, \"filename\" )")
        IR_NullStatement
      } else {
        val field = args.head.asInstanceOf[IR_FieldAccess]
        val fieldLayout = field.field.layout
        val numPoints = (0 until fieldLayout.numDimsGrid).map(dim =>
          fieldLayout.layoutsPerDim(dim).numDupLayersLeft + fieldLayout.layoutsPerDim(dim).numInnerLayers + fieldLayout.layoutsPerDim(dim).numDupLayersRight)
        val filename = args(1) //.asInstanceOf[IR_StringConstant].value

        val stmts = ListBuffer[IR_Statement]()

        stmts += HACK_IR_Native("cimg_library::CImg< double > imageOut ( " + numPoints.mkString(", ") + " )")
        stmts += IR_LoopOverPoints(field.field,
          IR_Assignment(HACK_IR_Native("*imageOut.data(i0,i1)"), field))

        // flip image for correct representation
        stmts += IR_MemberFunctionCall("imageOut", "mirror", HACK_IR_Native("'y'"))

        filename match {
          case va : IR_VariableAccess => stmts += IR_MemberFunctionCall("imageOut", "save", IR_MemberFunctionCall(va, "c_str"))
          case other                  => stmts += IR_MemberFunctionCall("imageOut", "save", other)
        }
        //stmts += HACK_IR_Native("imageOut.save( \"" + filename.value + "\" )")

        IR_Scope(stmts)
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("writeMappedImage", _), args)) =>
      if (args.size != 2 || !args.head.isInstanceOf[IR_FieldAccess]) {
        Logger.warn("Malformed call to writeMappedImage; usage: writeMappedImage ( field, \"filename\" )")
        IR_NullStatement
      } else {
        val field = args.head.asInstanceOf[IR_FieldAccess]
        val fieldLayout = field.field.layout
        var numPoints = (0 until fieldLayout.numDimsGrid).map(dim =>
          fieldLayout.layoutsPerDim(dim).numDupLayersLeft + fieldLayout.layoutsPerDim(dim).numInnerLayers + fieldLayout.layoutsPerDim(dim).numDupLayersRight).toList
        val filename = args(1) //.asInstanceOf[IR_StringConstant].value

        val stmts = ListBuffer[IR_Statement]()

        while (numPoints.length < 3) numPoints :+= 1
        // add color channels
        numPoints :+= 3

        stmts += HACK_IR_Native("cimg_library::CImg< double > imageOut ( " + numPoints.mkString(", ") + ", 1. )")
        stmts += IR_LoopOverPoints(field.field,
          IR_Assignment(HACK_IR_Native("*imageOut.data(i0,i1,0,0)"), 360.0 * field))

        stmts += IR_MemberFunctionCall("imageOut", "HSVtoRGB")
        // flip image for correct representation
        stmts += IR_MemberFunctionCall("imageOut", "mirror", HACK_IR_Native("'y'"))

        filename match {
          case va : IR_VariableAccess => stmts += IR_MemberFunctionCall("imageOut", "save", IR_MemberFunctionCall(va, "c_str"))
          case other                  => stmts += IR_MemberFunctionCall("imageOut", "save", other)
        }
        //stmts += HACK_IR_Native("imageOut.save( \"" + filename.value + "\" )")

        IR_Scope(stmts)
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("showImage", _), args)) =>
      if (0 == args.size || !args.map(_.isInstanceOf[IR_FieldAccess]).reduce(_ && _)) {
        Logger.warn("Malformed call to showImage; usage: showImage ( field.* )")
        IR_NullStatement
      } else {
        val fields = args.map(_.asInstanceOf[IR_FieldAccess])
        val fieldLayouts = fields.map(_.field.layout)
        val numPoints = fieldLayouts.map(fieldLayout => (0 until fieldLayout.numDimsGrid).map(dim =>
          fieldLayout.layoutsPerDim(dim).numDupLayersLeft + fieldLayout.layoutsPerDim(dim).numInnerLayers + fieldLayout.layoutsPerDim(dim).numDupLayersRight))

        val tmpImgs = fields.indices.map(i => s"imageShow$i")
        val displays = fields.indices.map(i => s"cImgDisp$i")

        val stmts = ListBuffer[IR_Statement]()

        for (i <- fields.indices) {
          stmts += HACK_IR_Native("cimg_library::CImg< double > " + tmpImgs(i) + " ( " + numPoints(i).mkString(", ") + " )")
          stmts += IR_LoopOverPoints(fields(i).field,
            IR_Assignment(HACK_IR_Native("*" + tmpImgs(i) + ".data(i0,i1)"), fields(i)))

          // flip image for correct representation
          stmts += IR_MemberFunctionCall(tmpImgs(i), "mirror", HACK_IR_Native("'y'"))

          val dispName = fields(i).field.name + "@" + fields(i).field.level
          stmts += HACK_IR_Native("cimg_library::CImgDisplay " + displays(i) + "(" + tmpImgs(i) + ", \"" + dispName + "\")")
        }
        stmts += IR_WhileLoop(fields.indices.map(i => IR_Negation(IR_MemberFunctionCall(displays(i), "is_closed")) : IR_Expression).reduceLeft(IR_OrOr),
          fields.indices.map(i => IR_MemberFunctionCall(displays(i), "wait") : IR_Statement).to[ListBuffer])

        IR_Scope(stmts)
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("showMappedImage", _), args)) =>
      if (0 == args.size || !args.map(_.isInstanceOf[IR_FieldAccess]).reduce(_ && _)) {
        Logger.warn("Malformed call to showImage; usage: showMappedImage ( field.* )")
        IR_NullStatement
      } else {
        val fields = args.map(_.asInstanceOf[IR_FieldAccess])
        val fieldLayouts = fields.map(_.field.layout)
        val numPoints = fieldLayouts.map(fieldLayout => (0 until fieldLayout.numDimsGrid).map(dim =>
          fieldLayout.layoutsPerDim(dim).numDupLayersLeft + fieldLayout.layoutsPerDim(dim).numInnerLayers + fieldLayout.layoutsPerDim(dim).numDupLayersRight).toList)

        val tmpImgs = fields.indices.map(i => s"imageShow$i")
        val displays = fields.indices.map(i => s"cImgDisp$i")

        val stmts = ListBuffer[IR_Statement]()

        for (i <- fields.indices) {
          while (numPoints(i).length < 3) numPoints(i) :+= 1
          // add color channels
          numPoints(i) :+= 3

          stmts += HACK_IR_Native("cimg_library::CImg< double > " + tmpImgs(i) + " ( " + numPoints(i).mkString(", ") + ", 1. )")
          stmts += IR_LoopOverPoints(fields(i).field,
            IR_Assignment(HACK_IR_Native("*" + tmpImgs(i) + ".data(i0,i1,0,0)"), 360.0 * fields(i)))

          stmts += IR_MemberFunctionCall(tmpImgs(i), "HSVtoRGB")
          // flip image for correct representation
          stmts += IR_MemberFunctionCall(tmpImgs(i), "mirror", HACK_IR_Native("'y'"))

          val dispName = fields(i).field.name + "@" + fields(i).field.level
          stmts += HACK_IR_Native("cimg_library::CImgDisplay " + displays(i) + "(" + tmpImgs(i) + ", \"" + dispName + "\")")
        }
        stmts += IR_WhileLoop(fields.indices.map(i => IR_Negation(IR_MemberFunctionCall(displays(i), "is_closed")) : IR_Expression).reduceLeft(IR_OrOr),
          fields.indices.map(i => IR_MemberFunctionCall(displays(i), "wait") : IR_Statement).to[ListBuffer])

        IR_Scope(stmts)
      }

    case IR_ExpressionStatement(IR_FunctionCall(IR_UnresolvedFunctionReference("showMappedImageAndWaitWhen", _), args)) =>
      if (args.size < 2 || !args.drop(1).map(_.isInstanceOf[IR_FieldAccess]).reduce(_ && _)) {
        Logger.warn("Malformed call to showMappedImageAndWaitWhen; usage: showMappedImageAndWaitWhen ( condition, field.* )")
        IR_NullStatement
      } else {
        val condition = args.head
        val fields = args.drop(1).map(_.asInstanceOf[IR_FieldAccess])
        val fieldLayouts = fields.map(_.field.layout)
        val numPoints = fieldLayouts.map(fieldLayout => (0 until fieldLayout.numDimsGrid).map(dim =>
          fieldLayout.layoutsPerDim(dim).numDupLayersLeft + fieldLayout.layoutsPerDim(dim).numInnerLayers + fieldLayout.layoutsPerDim(dim).numDupLayersRight).toList)

        val tmpImgs = fields.indices.map(i => s"imageShow$i")
        val displays = fields.indices.map(i => s"cImgDisp$i")

        val stmts = ListBuffer[IR_Statement]()

        for (i <- fields.indices) {
          while (numPoints(i).length < 3) numPoints(i) :+= 1
          // add color channels
          numPoints(i) :+= 3

          stmts += HACK_IR_Native("cimg_library::CImg< double > " + tmpImgs(i) + " ( " + numPoints(i).mkString(", ") + ", 1. )")
          stmts += IR_LoopOverPoints(fields(i).field,
            IR_Assignment(HACK_IR_Native("*" + tmpImgs(i) + ".data(i0,i1,0,0)"), 360.0 * fields(i)))

          stmts += IR_MemberFunctionCall(tmpImgs(i), "HSVtoRGB")
          // flip image for correct representation
          stmts += IR_MemberFunctionCall(tmpImgs(i), "mirror", HACK_IR_Native("'y'"))

          val scaledPoints = numPoints(i).take(2).map(i => { var ii = i; while (ii < 1000) { ii *= 2 }; ii : IR_Expression }).to[ListBuffer]
          stmts += IR_MemberFunctionCall(tmpImgs(i), "resize", scaledPoints)

          val dispName = fields(i).field.name + "@" + fields(i).field.level
          stmts += HACK_IR_Native("static cimg_library::CImgDisplay " + displays(i) + "(" + tmpImgs(i) + ", \"" + dispName + "\")")
          stmts += IR_Assignment(displays(i), tmpImgs(i))
        }
        stmts += IR_WhileLoop(IR_AndAnd(IR_Negation(condition), fields.indices.map(i => IR_Negation(IR_MemberFunctionCall(displays(i), "is_closed")) : IR_Expression).reduceLeft(IR_OrOr)),
          fields.indices.map(i => IR_MemberFunctionCall(displays(i), "wait") : IR_Statement).to[ListBuffer])

        IR_Scope(stmts)
      }
  })
}
