/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package util

import java.net.{ URL, MalformedURLException }
import scala.collection.{ mutable, immutable }
import io.{ File, Directory, Path, Jar, AbstractFile, ClassAndJarInfo }
import Jar.isJarOrZip
import File.pathSeparator
import ClassPathPrinter.{ Filter => CPFilter }
import ClassPath._
import scala.io.ClassProvision._
import annotation.tailrec

/*

/** A provider creator takes a String and turns it into something
 *  which supplies classes.
 */
trait StringBasedProvision extends (String => ClassRepProvider) {
  def apply(str: String): ClassRepProvider
}
object ExtDirProvision extends StringBasedProvision {
  def apply(path: String): MultiProvider = ???
}
object ConfigFileProvision extends StringBasedProvision {
  def apply(path: String): MultiProvider = ???
}
// object AetherProvision extends StringBasedProvision {
//   def apply(descr: String): ClassRepProvider = {
//     val mod = ModuleID fromDescriptor descr
//
//   }
// }
*/

class ModuleID(val organization: String, val name: String, val revision: String) {

}
object NoModuleId extends ModuleID("", "", "") { }

object ModuleID {
  def apply(org: String, name: String, rev: String): ModuleID = new ModuleID(org, name, rev)
  def apply(descriptor: String): ModuleID = {
    val segments = (descriptor split """\s*%+\s*""").toList
    segments match {
      case org :: name :: rev :: Nil  => apply(org, name, rev)
      case org :: name :: Nil         => apply(org, name, "latest.release")
      case _                          => NoModuleId
    }
  }
}

trait ProviderCreation {
  def fromUrls(urls: Seq[URL]): MultiProvider
  def fromUrl(url: URL): UrlProvider
  def fromPath(path: PathType): SinglePathProvider
  def fromClassPath(classPath: String): ClassPathProvider
  def fromJar(path: PathType): JarProvider
  def fromDir(path: PathType): DirectoryProvider
  def fromExtDir(path: PathType): MultiProvider
  def fromModule(moduleId: ModuleID): ClassRepProvider
}
object Provider extends ProviderCreation {
  def fromUrls(urls: Seq[URL]): MultiProvider             = ???
  def fromUrl(url: URL): UrlProvider                      = ???
  def fromPath(path: PathType): SinglePathProvider        = ???
  def fromClassPath(classPath: String): ClassPathProvider = ???
  def fromJar(path: PathType): JarProvider                = ???
  def fromDir(path: PathType): DirectoryProvider          = ???
  def fromExtDir(path: PathType): MultiProvider           = ???
  def fromModule(moduleId: ModuleID): ClassRepProvider    = ???
}

trait BaseProvider extends ClassRepProvider {
  def pathAsURL(p: PathType)   = new URL("" + p)
  def sourceCode(name: String) = classRep(name).source
  def byteCode(name: String)   = classRep(name).binary
}
trait BasePackageProvider extends BaseProvider with PackageProvider {
  /** Maps from names to entities.  These are implemented with
   *  vals in the concrete classes.
   */
  def classMap: Map[String, ClassRep]
  def packageMap: Map[String, BasePackageProvider]

  def classes: Iterable[ClassRep]             = classMap.values
  def packages: Iterable[BasePackageProvider] = packageMap.values
  def providesClass(name: String)             = true

  /**
   * Find a ClassRep given a class name of the form "package.subpackage.ClassName".
   */
  def classRep(name: String): ClassRep = {
    val idx = name indexOf '.'
    // If no '.' present, lookup the simple name in the class map.
    // Otherwise, given foo.bar.baz, lookup 'foo' in the package map
    // and then call findClass("bar.baz") on the classpath instance.
    if (idx < 0) {
      if (classMap contains name) classMap(name)
      else NoClassRep
    }
    else {
      val pkg = name.substring(0, idx)
      if (packageMap contains pkg)
        packageMap(pkg) classRep name.substring(idx + 1)
      else NoClassRep
    }
  }
}

trait MultiProvider extends BaseProvider {
  def providers: List[ClassRepProvider]

  def flatten: List[ClassRepProvider] = providers flatMap {
    case mp: MultiProvider  => mp.flatten
    case p                  => List(p)
  }
}

trait CompilerClassProvider extends PackageProvider with BaseProvider with HasClassPath {
  def classPathUrls: List[URL]
}

trait HasClassPath {
  def classPathUrls: List[URL] = ClassPath toURLs classPath
  def classPath: String
}

trait SinglePathProvider extends CompilerClassProvider with UrlProvider {
  def path: PathType

  def classPath     = "" + path
  def url           = pathAsURL(path)
  override def classPathUrls = List(url)
}
trait UrlProvider extends BaseProvider {
  def url: URL
}

trait ClassPathProvider extends MultiProvider with HasClassPath {
  def providers: List[SinglePathProvider] = ClassPath toPaths classPath map (Provider fromPath "" + _)
  override def classPathUrls              = providers map (_.url)
}

abstract class PackageTraverser {
  import scala.io.internal._

  val pbuf = mutable.HashMap[String, DirOrJarProvider]()
  val cbuf = mutable.HashMap[String, ClassRep]()

  def contents: Iterable[AbstractFile]
  def includeSource: Boolean
  def newSubPackage(path: AbstractFile): DirOrJarProvider
  def isValidPackage(name: String) = (name != "") && (name != "META-INF")

  protected def newClassRep(entry: AbstractFile, ext: String): ClassRep = {
    if (ext == "class")
      BinaryRep(ClassBinaryFile(entry))
    else if (includeSource && (ext == "java" || ext == "scala"))
      SourceRep(ClassSourceFile(entry))
    else
      null
  }
  protected def traverseEntry(entry: AbstractFile) {
    val idx = entry.name lastIndexOf '.'
    if (idx < 0) {
      if (entry.isDirectory && isValidPackage(entry.name))
        pbuf(entry.name) = newSubPackage(entry)
    }
    else {
      val cr = newClassRep(entry, entry.name.substring(idx + 1))
      if (cr ne null)
        cbuf(cr.className) = cr
    }
  }
  // calculates (packageMap, classMap) in one traversal.
  def traverse(): (Map[String, DirOrJarProvider], Map[String, ClassRep]) = {
    contents foreach traverseEntry
    (pbuf.toMap, cbuf.toMap withDefaultValue NoClassRep)
  }
}

trait DirOrJarProvider extends BasePackageProvider {
  outer =>

  def dir: AbstractFile
  def newTraverser(): PackageTraverser

  def path        = dir.path
  def packageName = dir.name
  lazy val (packageMap, classMap) = newTraverser().traverse()

  override def toString() = "" + path
}

class DirectoryProvider(val dir: AbstractFile) extends DirOrJarProvider {
  def newTraverser() = new PackageTraverser {
    def contents                         = dir
    def includeSource                    = false
    def newSubPackage(sub: AbstractFile) = new DirectoryProvider(sub)
  }
}
class SourcePathProvider(val dir: AbstractFile) extends DirOrJarProvider {
  def newTraverser() = new PackageTraverser {
    def contents                         = dir
    def includeSource                    = true
    def newSubPackage(sub: AbstractFile) = new SourcePathProvider(sub)
  }
}

abstract class JarProvider(val path: PathType) extends SinglePathProvider {

}

trait SerialClassProvider extends MultiProvider {
  def classRep(name: String) = {
    @tailrec def loop(xs: List[ClassRepProvider]): ClassRep = xs match {
      case Nil      => NoClassRep
      case x :: xs  => if (x providesClass name) x classRep name else loop(xs)
    }
    loop(providers)
  }

  def providesClass(name: String) = providers exists (_ providesClass name)
}

class TransitionPackageProvider(underlying: ClassPath) extends CompilerClassProvider {
  lazy val classes  = underlying.classes
  lazy val packages = underlying.packages map (p => new TransitionPackageProvider(p))
  // def deepClasses   = classes ++ packages.flatMap(_.classes)
  // def deepPackages  = packages ++ packages.flatMap(_.packages)

  def classRep(name: String)      = underlying classRep name
  def providesClass(name: String) = underlying providesClass name
  def asURLs                      = underlying.asURLs
  def classPath                   = underlying.classPathString
  def packageName                 = underlying.name

  override def toString    = packageName
}

//
// object standard {
//   private def findBootClasspath = propOrElse(
//     "sun.boot.class.path",
//     systemProperties collectFirst { case (k, v) if k endsWith ".boot.class.path" => v } getOrElse ""
//   )
//
//     systemProperties find (_._1 endsWith ".boot.class.path") map (_._2) getOrElse ""
//
//   def fromClassPath
//   def fromPath
//   def scalaHome(path: Path) = allJarsIn(path + "/lib")
//   def scalaHome()           = scalaHome(propOrEmpty("scala.home"))
//   def bootClasspath =
//
//   def
//
// def scalaHomeProvider =
// def boot
//
// class ScalaHomeProvider(scalaHome: Path) extends PathBasedProvider {
//
//
// trait ScalaHomeProvider extends PathBasedProvider {
//   def scalaHome: Path = propOrEmpty("scala.home")
// }
//
// trait PathProvider extends ClassRepProvider {
//   def path: Path
// }
//
// trait DirBasedProvider extends PathProvider {
//
// }
// trait JarBasedProvider extends PathProvider {
//
// }
// trait Property
//
// class JavaBootClasspath extends PropertyBasedProvider("sun.boot.class.path")
// class JavaUserClasspath extends PropertyBasedProvider("java.class.path")
//
//
//
// def javaBootClassPath   = propOrElse("sun.boot.class.path", searchForBootClasspath)
// def javaExtDirs         = propOrEmpty("java.ext.dirs")
// def scalaHome           = propOrEmpty("scala.home")
// def scalaExtDirs        = propOrEmpty("scala.ext.dirs")
//
//
// // Assemble the elements!
// def basis = List[Traversable[ClassPath]](
//   classesInPath(javaBootClassPath),             // 1. The Java bootstrap class path.
//   contentsOfDirsInPath(javaExtDirs),            // 2. The Java extension class path.
//   classesInExpandedPath(javaUserClassPath),     // 3. The Java application class path.
//   classesInPath(scalaBootClassPath),            // 4. The Scala boot class path.
//   contentsOfDirsInPath(scalaExtDirs),           // 5. The Scala extension class path.
//   classesInExpandedPath(userClassPath),         // 6. The Scala application class path.
//   sourcesInPath(sourcePath)                     // 7. The Scala source path.
// )
//
//

class ForwardingPackageProvider(underlying: PackageProvider) extends PackageProvider {
  def classes: Iterable[ClassRep]          = underlying classes
  def classRep(name: String): ClassRep     = underlying classRep name
  def providesClass(name: String): Boolean = underlying providesClass name
  def packageName: String                  = underlying packageName
  def packages: Iterable[PackageProvider]  = underlying packages
}

class FilteredPackageProvider(underlying0: PackageProvider, p: String => Boolean) extends ForwardingPackageProvider(underlying0) {
  override def classRep(name: String): ClassRep         = if (p(name)) super.classRep(name) else NoClassRep
  override lazy val classes: Iterable[ClassRep]         = super.classes filter (c => p(c.className))
  override lazy val packages: Iterable[PackageProvider] = super.packages map (pkg => new FilteredPackageProvider(pkg, p))
}

/**
 *  This module provides star expansion of '-classpath' option arguments, behaves the same as
 *  java, see [http://java.sun.com/javase/6/docs/technotes/tools/windows/classpath.html]
 *
 *  @author Stepan Koltsov
 */
object ClassPath {
  def scalaLibrary  = locate[Option[_]]
  def scalaCompiler = locate[Global]

  def infoFor[T](value: T)        = info(value.getClass)
  def info[T](clazz: Class[T])    = new ClassAndJarInfo()(ClassManifest fromClass clazz)
  def info[T: ClassManifest]      = new ClassAndJarInfo[T]
  def locate[T: ClassManifest]    = info[T] rootClasspath
  def locateJar[T: ClassManifest] = info[T].rootPossibles find (x => isJarOrZip(x)) map (x => File(x))
  def locateDir[T: ClassManifest] = info[T].rootPossibles find (_.isDirectory) map (_.toDirectory)

  /** Expand single path entry */
  private def expandS(pattern: String): List[String] = {
    val wildSuffix = File.separator + "*"

    /** Get all subdirectories, jars, zips out of a directory. */
    def lsDir(dir: Directory, filt: String => Boolean = _ => true) =
      dir.list filter (x => filt(x.name) && (x.isDirectory || isJarOrZip(x))) map (_.path) toList

    def basedir(s: String) =
      if (s contains File.separator) s.substring(0, s.lastIndexOf(File.separator))
      else "."

    if (pattern == "*") lsDir(Directory("."))
    else if (pattern endsWith wildSuffix) lsDir(Directory(pattern dropRight 2))
    else if (pattern contains '*') {
      val regexp = ("^%s$" format pattern.replaceAll("""\*""", """.*""")).r
      lsDir(Directory(pattern).parent, regexp findFirstIn _ isDefined)
    }
    else List(pattern)
  }

  /** Split classpath using platform-dependent path separator */
  def split(path: String): List[String] = (path split pathSeparator).toList filterNot (_ == "") distinct

  /** Join classpath using platform-dependent path separator */
  def join(paths: String*): String  = paths filterNot (_ == "") mkString pathSeparator

  /** Split the classpath, apply a transformation function, and reassemble it. */
  def map(cp: String, f: String => String): String = join(split(cp) map f: _*)

  /** Split the classpath, filter according to predicate, and reassemble. */
  def filter(cp: String, p: String => Boolean): String = join(split(cp) filter p: _*)

  /** Split the classpath and map them into Paths */
  def toPaths(cp: String): List[Path] = split(cp) map (x => Path(x).toAbsolute)

  /** Make all classpath components absolute. */
  def makeAbsolute(cp: String): String = fromPaths(toPaths(cp): _*)

  /** Join the paths as a classpath */
  def fromAbFiles(paths: AbstractFile*): String = join(paths map (_.path): _*)
  def fromPaths(paths: Path*): String = join(paths map (_.path): _*)
  def fromURLs(urls: URL*): String = fromPaths(urls map (x => Path(x.getPath)) : _*)

  /** Split the classpath and map them into URLs */
  def toURLs(cp: String): List[URL] = toPaths(cp) map (_.toURL)

  /** Expand path and possibly expanding stars */
  def expandPath(path: String, expandStar: Boolean = true): List[String] =
    if (expandStar) split(path) flatMap expandS
    else split(path)

  /** Expand dir out to contents, a la extdir */
  def expandDir(extdir: String): List[String] = {
    AbstractFile getDirectory extdir match {
      case null => Nil
      case dir  => dir filter (_.isClassContainer) map (x => new java.io.File(dir.file, x.name) getPath) toList
    }
  }
  /** Expand manifest jar classpath entries: these are either urls, or paths
   *  relative to the location of the jar.
   */
  def expandManifestPath(jarPath: String): List[URL] = {
    val file = File(jarPath)
    if (!file.isFile) return Nil

    val baseDir = file.parent
    new Jar(file).classPathElements map (elem =>
      specToURL(elem) getOrElse (baseDir / elem).toURL
    )
  }

  /** A useful name filter. */
  def isTraitImplementation(name: String) = name endsWith "$class.class"

  def specToURL(spec: String): Option[URL] =
    try Some(new URL(spec))
    catch { case _: MalformedURLException => None }

  trait Logic {
    outer =>

    def newPath(dir: AbstractFile, isSourcePath: Boolean): ClassPath   = new DirectoryClassPath(dir, isSourcePath) { override def logic = outer }
    def newClassPath(dir: AbstractFile): ClassPath                     = newPath(dir, isSourcePath = false)
    def newSourcePath(dir: AbstractFile): ClassPath                    = newPath(dir, isSourcePath = true)
    def newMergedPath(entries: IndexedSeq[ClassPath]): MergedClassPath = new MergedClassPath(entries) { override def logic = outer }

    // def isBinaryName(name: String)  = (name endsWith ".class")
    // def isSourceName(name: String)  = (name endsWith ".scala") || (name endsWith ".java")
    def isPackageName(name: String) = (name != "META-INF") && (name != "") && !(name contains '.')

    def sourcesInPath(path: String): List[ClassPath] =
      for (file <- expandPath(path, false) ; dir <- Option(AbstractFile getDirectory file)) yield
        newSourcePath(dir)

    def contentsOfDirsInPath(path: String): List[ClassPath] =
      for (dir <- expandPath(path, false) ; name <- expandDir(dir) ; entry <- Option(AbstractFile getDirectory name)) yield
        newClassPath(entry)

    def classesAtAllURLS(path: String): List[ClassPath] =
      (path split " ").toList flatMap classesAtURL

    def classesAtURL(spec: String) =
      for (url <- specToURL(spec).toList ; location <- Option(AbstractFile getURL url)) yield
        newClassPath(location)

    def classesInExpandedPath(path: String): IndexedSeq[ClassPath] =
      classesInPathImpl(path, true).toIndexedSeq

    def classesInPath(path: String) = classesInPathImpl(path, false)

    // Internal
    private def classesInPathImpl(path: String, expand: Boolean) =
      for (file <- expandPath(path, expand) ; dir <- Option(AbstractFile getDirectory file)) yield
        newClassPath(dir)
  }

  implicit def toPackageProvider(cp: ClassPath) = new TransitionPackageProvider(cp)

  // class JarProvider(jar: java.io.File) extends PackageProvider
  //
  // class TransitionPackageProvider(underlying: ClassPath) extends CompilerClassProvider {
  //   lazy val classes  = underlying.classes
  //   lazy val packages = underlying.packages map (p => new TransitionPackageProvider(p))
  //
  //   def deepClasses   = classes ++ packages.flatMap(_.classes)
  //   def deepPackages  = packages ++ packages.flatMap(_.packages)
  //
  //   def classRep(name: String) = underlying classRep name
  //   def providesClass(name: String) = underlying providesClass name
  //   def asURLs                 = underlying.asURLs
  //   def classPathString        = underlying.classPathString
  //   def packageName            = underlying.name
  //   def sourcePathString       = underlying.sourcePathString
  //
  //   override def toString    = packageName
  // }
}

/**
 * Represents a package which contains classes and other packages
 */
abstract class ClassPath extends ClassRepProvider {
  outer =>

  def logic: ClassPath.Logic

  /**
   * The short name of the package (without prefix)
   */
  def name: String

  /**
   * A String representing the origin of this classpath element, if known.
   * For example, the path of the directory or jar.
   */
  def origin: Option[String] = None

  /** A list of URLs representing this classpath.
   */
  def asURLs: List[URL]

  /** The whole classpath in the form of one String.
   */
  def asClasspathString: String

  def isSourcePath: Boolean

  def flatten: Seq[ClassPath] = this :: Nil

  def sourcePathFiles  = flatten collect { case x: DirectoryClassPath if  x.isSourcePath => x.dir }
  def classPathFiles   = flatten collect { case x: DirectoryClassPath if !x.isSourcePath => x.dir }
  def classPathString  = ClassPath.fromAbFiles(classPathFiles: _*)

  /** Maps from names to entities.  These are implemented with
   *  vals in the concrete classes.
   */
  def classMap: Map[String, ClassRep]
  def packageMap: Map[String, ClassPath]
  // def sourcesMap: Map[String, AbstractFile]

  def providesClass(name: String) = true //name endsWith ".class"
  // classRep(name) ne NoClassRep

  def classes  = classMap.values
  def packages = packageMap.values
  // def sources  = sourcesMap.values

  @deprecated("Use classRep", "2.10.0")
  def findClass(name: String): Option[ClassRep] = {
    val idx = name indexOf '.'
    // If no '.' present, lookup the simple name in the class map.
    // Otherwise, given foo.bar.baz, lookup 'foo' in the package map
    // and then call findClass("bar.baz") on the classpath instance.
    if (idx < 0) classMap get name
    else packageMap get name.substring(0, idx) flatMap (_ findClass name.substring(idx + 1))
  }

  /**
   * Find a ClassRep given a class name of the form "package.subpackage.ClassName".
   * Does not support nested classes on .NET
   */
  def classRep(name: String): ClassRep = {
    val idx = name indexOf '.'
    // If no '.' present, lookup the simple name in the class map.
    // Otherwise, given foo.bar.baz, lookup 'foo' in the package map
    // and then call findClass("bar.baz") on the classpath instance.
    if (idx < 0) {
      if (classMap contains name) classMap(name)
      else NoClassRep
    }
    else {
      val pkg = name.substring(0, idx)
      if (packageMap contains pkg)
        packageMap(pkg) classRep name.substring(idx + 1)
      else NoClassRep
    }
  }
  def byteCode(name: String): ClassBinaryFile = classRep(name).binary
  def sourceCode(name: String): ClassSourceFile = classRep(name).source

  @deprecated("Use `byteCode` to find classfiles and `sourceCode` to find source code", "2.10.0")
  def findSourceFile(name: String) = byteCode(name)

  def sortString = join(split(asClasspathString).sorted: _*)
  override def equals(that: Any) = that match {
    case x: ClassPath  => this.sortString == x.sortString
    case _                => false
  }
  override def hashCode = sortString.hashCode()

  def showIf[Fn](pred: Fn)(implicit f: Fn => CPFilter) {
    ClassPathPrinter.show(this, f(pred))
  }
}

/** A directory (or a .jar file) containing classfiles and packages
 */
abstract class DirectoryClassPath(val dir: AbstractFile, val isSourcePath: Boolean) extends ClassPath {
  def name = dir.name
  override def origin = dir.underlyingSource map (_.path)
  def asURLs = if (dir.file == null) Nil else List(dir.toURL)
  def asClasspathString = dir.path
  // def sourcePaths = if (isSourcePath) Iterable(dir) else Nil

  // calculates (packageMap, classMap, sourcesMap) in one traversal.
  private def traverse() = {
    import scala.io.internal._
    val cbuf = mutable.HashMap[String, ClassRep]()
    val pbuf = mutable.HashMap[String, ClassPath]()

    dir foreach { f =>
      if (f.isDirectory) {
        if (logic.isPackageName(f.name))
          pbuf(f.name) = logic.newPath(f, isSourcePath)
      }
      else if (f hasExtension "class") {
        val cr = BinaryRep(ClassBinaryFile(f))
        cbuf(cr.className) = cr
      }
      else if (isSourcePath && ((f hasExtension "java") || (f hasExtension "scala"))) {
        val cr = SourceRep(ClassSourceFile(f))
        cbuf(cr.className) = cr
      }
    }

    (pbuf.toMap, cbuf.toMap)
  }

  lazy val (packageMap, classMap) = traverse()
  override def toString() = "directory classpath: "+ origin.getOrElse("?")
}

/**
 * A classpath unifying multiple class- and sourcepath entries.
 */
abstract class MergedClassPath(val entries: IndexedSeq[ClassPath]) extends ClassPath {
  def this(entries: TraversableOnce[ClassPath]) = this(entries.toIndexedSeq)

  def name   = entries.head.name
  def asURLs = entries flatMap (_.asURLs) toList
  // def sourcePaths = entries flatMap (_.sourcePaths)
  def isSourcePath = false

  override def flatten: Seq[ClassPath] = entries flatMap (_.flatten)
  override def origin = Some(entries map (x => x.origin getOrElse x.name) mkString ("Merged(", ", ", ")"))
  override def asClasspathString: String = join(entries map (_.asClasspathString) : _*)

  private def traverse() = {
    val cbuf = mutable.HashMap[String, ClassRep]() withDefaultValue null
    val pbuf = mutable.HashMap[String, ClassPath]() withDefaultValue null

    for (e <- entries) {
      for ((name, c) <- e.classMap) {
        cbuf(name) match {
          case null                          => cbuf(name) = c
          case SourceRep(src) if c.hasBinary => cbuf(name) = DualClassRep(src, c.binary)
          case BinaryRep(bin) if c.hasSource => cbuf(name) = DualClassRep(c.source, bin)
          case _                             => ()
        }
      }
      for ((name, p) <- e.packageMap) {
        pbuf(name) = (pbuf(name) match {
          case null                => p
          case cp: MergedClassPath => logic.newMergedPath(cp.entries :+ p)
          case to                  => logic.newMergedPath(IndexedSeq(to, p))
        })
      }
    }

    (pbuf.toMap, cbuf.toMap)
  }

  lazy val (packageMap, classMap) = traverse()

  def show() {
    println("ClassPath %s has %d entries and results in:\n".format(name, entries.size))
    asClasspathString split ':' foreach (x => println("  " + x))
  }
  override def toString() = "merged classpath "+ entries.mkString("(", "\n", ")")
}
