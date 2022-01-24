//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.polyhedron

import scala.util.Random

import java.io._
import java.net.URL

import exastencils.logger.Logger

object Isl {

  // register some immutables (warning: this is incomplete!)
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[isl.Ctx])
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[isl.UnionSet])
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[isl.UnionMap])
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[isl.Val])

  private var loaded : Boolean = false
  def load() : Unit = {

    if (loaded)
      return

    val system : String =
      System.getProperty("os.name") match {
        case x if x.startsWith("Windows") => "win32"
        case x if x.startsWith("Linux")   => "linux"
        case x if x.startsWith("Mac")     => "darwin"
        case x if x.startsWith("Darwin")  => "darwin"
        case x                            =>
          throw new Exception("unknown operating system (" + x + "), cannot load native library isl")
      }
    val arch : String =
      System.getProperty("os.arch") match {
        case "amd64"     => "x86_64"
        case "x86_64"    => "x86_64"
        case "i386"      => "x86"
        case "x86"       => "x86"
        case "powerpc64" => "ppc64"
        case "aarch64"   => "arm64"
        case x           =>
          throw new Exception("unknown system architecture (" + x + "), cannot load native library isl")
      }

    val ldir : String = system + '-' + arch
    val lname : String = System.mapLibraryName("isl_jni")
    val lurl : URL = ClassLoader.getSystemResource(ldir + '/' + lname)
    if (lurl == null)
      Logger.error("unable to locate native library " + ldir + '/' + lname)

    val markerSuffix : String = ".del"
    val tmpDir = new java.io.File(System.getProperty("java.io.tmpdir"), "exastencils_native-libs-" + System.getProperty("user.name").hashCode())
    if (tmpDir.exists()) {
      // remove old libs
      val markers : Array[java.io.File] =
        tmpDir.listFiles(new java.io.FilenameFilter() {
          def accept(dir : java.io.File, name : String) : Boolean = {
            name.endsWith(markerSuffix)
          }
        })
      for (m <- markers) {
        var oldLibName : String = m.getName()
        oldLibName = oldLibName.substring(0, oldLibName.length() - markerSuffix.length())
        val oldLibDir = new java.io.File(tmpDir, oldLibName)
        for (f <- oldLibDir.listFiles())
          f.delete()
        if (!oldLibDir.exists() || oldLibDir.delete())
          m.delete()
      }
    }

    lurl.getProtocol match {
      case "file" =>
        val lfile : String = lurl.getPath()
        val lpath : String = lfile.substring(0, lfile.lastIndexOf('/'))
        isl.Init.loadNative(lpath)

      case "jar" =>
        if (!tmpDir.exists())
          tmpDir.mkdir()
        var tmpIslLibDir : java.io.File = null
        var repeat : Boolean = true
        val rnd = new Random()
        var i : Int = 0
        do {
          i += 1
          var suffix = rnd.nextLong()
          if (suffix == Long.MinValue)
            suffix = 0
          else
            suffix = math.abs(suffix)
          tmpIslLibDir = new java.io.File(tmpDir, "tmp" + suffix)
          repeat = !tmpIslLibDir.mkdir()
        } while (repeat && i < 1000)
        if (repeat)
          Logger.error("unable to create temp directory for native lib in " + tmpDir.getAbsolutePath)
        val tmpIslLib = new java.io.File(tmpIslLibDir, lname)
        tmpIslLib.createNewFile()

        val is : InputStream = lurl.openStream()
        val fos = new FileOutputStream(tmpIslLib)

        val buffer = new Array[Byte](65536) // 64 KB
        try {
          Stream.continually(is.read(buffer))
            .takeWhile(_ > 0)
            .foreach { l => fos.write(buffer, 0, l) }
        } finally {
          fos.flush()
          fos.close()
          is.close()
        }

        isl.Init.loadNative(tmpIslLibDir.getAbsolutePath)

        // library loaded, try to delete file now (which should be possible on linux, but not on windows)
        if (!(tmpIslLib.delete() && tmpIslLibDir.delete())) {
          // cannot delete temp lib now, mark for later deletion (either when jvm exits normally, or on next run)
          val marker = new java.io.File(tmpDir, tmpIslLibDir.getName + markerSuffix)
          marker.createNewFile()
          tmpIslLib.deleteOnExit()
        }
    }

    loaded = true
  }

  def newCtx() : isl.Ctx = {
    this.load()
    isl.Ctx.alloc()
  }

  // lazy not required here, this class gets loaded only, if anything inside it is used (and it contains only isl related stuff)
  final val ctx : isl.Ctx = newCtx()

  // names are intended to be imported (using "import exastencils.polyhedron.Isl.TypeAliases._")
  object TypeAliases {
    final val T_PAR = isl.DimType.Param
    final val T_SET = isl.DimType.Set
    final val T_IN = isl.DimType.In
    final val T_OUT = isl.DimType.Out
  }

  def simplify(uset : isl.UnionSet) : isl.UnionSet = {
    if (uset == null)
      return null
    uset.coalesce()
  }

  def simplify(umap : isl.UnionMap) : isl.UnionMap = {
    if (umap == null)
      return null
    umap.coalesce()
  }

  def simplify(set : isl.Set) : isl.Set = {
    if (set == null)
      return null
    set.coalesce()
  }

  def simplify(map : isl.Map) : isl.Map = {
    if (map == null)
      return null
    map.coalesce()
  }

  def simplify(set : isl.BasicSet) : isl.BasicSet = {
    if (set == null)
      return null
    set.removeRedundancies()
  }

  def simplify(map : isl.BasicMap) : isl.BasicMap = {
    if (map == null)
      return null
    map.removeRedundancies()
  }
}
