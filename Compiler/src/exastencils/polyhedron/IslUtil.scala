package exastencils.polyhedron

import isl.Conversions._
import java.io.FileOutputStream
import java.io.InputStream

object Isl {

  // register some immutables (warning: this is incomplete!)
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[isl.Ctx])
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[isl.UnionSet])
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[isl.UnionMap])
  exastencils.core.Duplicate.dontCloneHierarchy(classOf[isl.Val])

  private var loaded : Boolean = false
  def load() : Unit = {

    if (loaded) return

    val system : String =
      System.getProperty("os.name") match {
        case x if (x.startsWith("Windows")) => "win32"
        case x if (x.startsWith("Linux"))   => "linux"
        case x if (x.startsWith("Mac"))     => "darwin"
        case x if (x.startsWith("Darwin"))  => "darwin"
        case x =>
          throw new Exception("unknown operating system (" + x + "), cannot load native library isl")
      }
    val arch : String =
      System.getProperty("os.arch") match {
        case "amd64"     => "x86-64"
        case "x86_64"    => "x86-64"
        case "i386"      => "x86"
        case "powerpc64" => "ppc64"
        case x =>
          throw new Exception("unknown system architecture (" + x + "), cannot load native library isl")
      }

    val dir : String = if (system == "darwin") system else system + '-' + arch

    val markerSuffix : String = ".del"

    var Array(fname, fext) = System.mapLibraryName("isl").split('.')
    if (system == "darwin" && fext == "jnilib")
      fext = "dylib"
    val is : InputStream = ClassLoader.getSystemResourceAsStream(dir + '/' + fname + '.' + fext)

    val tmpLibDir = new java.io.File(System.getProperty("java.io.tmpdir"), "exastencils_native-libs")
    if (tmpLibDir.exists()) {
      // remove old libs
      val markers : Array[java.io.File] =
        tmpLibDir.listFiles(new java.io.FilenameFilter() {
          def accept(dir : java.io.File, name : String) : Boolean = {
            return name.endsWith(markerSuffix)
          }
        })
      for (m <- markers) {
        var oldLibName : String = m.getName()
        oldLibName = oldLibName.substring(0, oldLibName.length() - markerSuffix.length())
        val oldLib = new java.io.File(tmpLibDir, oldLibName)
        if (!oldLib.exists() || oldLib.delete())
          m.delete()
      }
    } else
      tmpLibDir.mkdir()
    val tmpIslLib = java.io.File.createTempFile(fname, '.' + fext, tmpLibDir)
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

    System.load(tmpIslLib.getAbsolutePath())

    // library loaded, try to delete file now (which should be possible on linux, but not on windows)
    if (!tmpIslLib.delete()) {
      // cannot delete temp lib now, mark for later deletion (either when jvm exits normally, or on next run)
      val marker = new java.io.File(tmpLibDir, tmpIslLib.getName() + markerSuffix)
      marker.createNewFile()
      tmpIslLib.deleteOnExit()
    }

    loaded = true
  }

  def initCtx() : isl.Ctx = {
    this.load()
    return isl.Ctx.alloc()
  }

  final lazy val ctx = initCtx()

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
    return uset.coalesce()
  }

  def simplify(umap : isl.UnionMap) : isl.UnionMap = {
    if (umap == null)
      return null
    return umap.coalesce()
  }

  def simplify(set : isl.Set) : isl.Set = {
    if (set == null)
      return null
    return set.coalesce()
  }

  def simplify(map : isl.Map) : isl.Map = {
    if (map == null)
      return null
    return map.coalesce()
  }

  def simplify(set : isl.BasicSet) : isl.BasicSet = {
    if (set == null)
      return null
    return set.removeRedundancies()
  }

  def simplify(map : isl.BasicMap) : isl.BasicMap = {
    if (map == null)
      return null
    return map.removeRedundancies()
  }
}
