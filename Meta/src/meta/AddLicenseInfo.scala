package meta

object AddLicenseInfo {
  val licenseInfo : String =
    """//=============================================================================
      |//
      |//  This file is part of the ExaStencils code generation framework. ExaStencils
      |//  is free software: you can redistribute it and/or modify it under the terms
      |//  of the GNU General Public License as published by the Free Software
      |//  Foundation, either version 3 of the License, or (at your option) any later
      |//  version.
      |//
      |//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
      |//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
      |//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
      |//  more details.
      |//
      |//  You should have received a copy of the GNU General Public License along
      |//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
      |//
      |//=============================================================================
      |""".stripMargin.replaceAll("\r\n", "\n")

  def recursivelyListFiles(f : java.io.File) : Array[java.io.File] = {
    val (files, folders) = f.listFiles.partition(_.isFile)
    files.filter(_.getName.endsWith(".scala")) ++ folders.flatMap(recursivelyListFiles)
  }

  def collectApplicableFiles() : Array[String] = {
    val folders = Array("./Compiler/src/", "./CompilerMacros/src/", "./ConfigEvaluator/src/", "./ConfigRunner/src/", "./Meta/src/")
    folders.flatMap(path => recursivelyListFiles(new java.io.File(path)).map(_.getPath.replace('\\', '/')))
  }

  def apply() : Unit = {
    val files = collectApplicableFiles()

    files.foreach(filename => {
      val source = scala.io.Source.fromFile(filename)
      val input = source.mkString.replaceAll("\r\n", "\n")
      source.close()

      if (!input.startsWith(licenseInfo)) {
        var output = ""
        output += licenseInfo
        output += "\n"
        output += input
        Generatable.printToFile(filename, output)
      }
    })
  }
}
