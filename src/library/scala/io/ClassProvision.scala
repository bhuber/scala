/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.io

import java.net.URL


/** Explain the whole translation process. */



object ClassProvision {
  // TODO - decide on what sort of Path we want.
  type PathType = String

  trait ClassRepFile {
    def name: String
    def className: String
    def path: PathType
    def lastModified: Long
    // def isEmpty: Boolean
    def bytes: Array[Byte]
  }
  trait ClassSourceFile extends ClassRepFile {
    def chars: Array[Char]
  }
  trait ClassBinaryFile extends ClassRepFile {
    // def binaryName: String
  }

  abstract class NoClassRepFile extends ClassRepFile {
    val bytes        = Array[Byte]()
    def className    = ""
    // def isEmpty      = true
    def lastModified = 0L
    def name         = ""
    def path         = ""
  }
  object NoClassSourceFile extends NoClassRepFile with ClassSourceFile {
    val chars = Array[Char]()
    override def toString = "<no source file>"
  }
  object NoClassBinaryFile extends NoClassRepFile with ClassBinaryFile {
    override def toString = "<no binary file>"
  }

  /** Represents entities which may have a source code representation, a
   *  binary representation, or both.  The interfaces ClassSourceFile
   *  and ClassBinaryFile are minimal so as to limit representation dependence.
   *
   *  The four subclasses SourceRep, BinaryRep, DualClassRep, and NoClassRep
   *  cover all permutations of source and binary.  The absence of source or
   *  binary is represented by the distinguished objects NoClassSourceFile and
   *  NoClassBinaryFile.
   */
  sealed abstract class ClassRep {
    def source: ClassSourceFile
    def binary: ClassBinaryFile

    def className  = if (hasBinary) binary.className else source.className
    def hasBinary  = binary ne NoClassBinaryFile
    def hasSource  = source ne NoClassSourceFile
    def sourceOnly = hasSource && !hasBinary
    def binaryOnly = hasBinary && !hasSource
    def isEmpty    = !hasBinary && !hasSource

    def orElse[T](alt: => ClassRep): ClassRep        = this
    def foreach[U](f: ClassRep => U): Unit           = f(this)
    def withFilter(p: ClassRep => Boolean): ClassRep = if (p(this)) this else NoClassRep
    def filter(p: ClassRep => Boolean): ClassRep     = if (p(this)) this else NoClassRep

    override def toString = className + ((source, binary))
  }
  final case class SourceRep(source: ClassSourceFile) extends ClassRep {
    require(source ne NoClassSourceFile, this)

    def binary = NoClassBinaryFile
    override def hasSource = true
  }
  final case class BinaryRep(binary: ClassBinaryFile) extends ClassRep {
    require(binary ne NoClassBinaryFile, this)

    def source = NoClassSourceFile
    override def hasBinary = true
  }
  final case class DualClassRep(source: ClassSourceFile, binary: ClassBinaryFile) extends ClassRep {
    require((source ne NoClassSourceFile) && (source ne NoClassSourceFile), this)
  }
  final case object NoClassRep extends ClassRep {
    def binary     = NoClassBinaryFile
    def source     = NoClassSourceFile

    override def hasBinary = false
    override def hasSource = false
    override def isEmpty   = true

    override def orElse[T](alt: => ClassRep): ClassRep        = alt
    override def foreach[U](f: ClassRep => U): Unit           = ()
    override def withFilter(p: ClassRep => Boolean): ClassRep = NoClassRep
    override def filter(p: ClassRep => Boolean): ClassRep     = NoClassRep

    override def toString = "NoClassRep"
  }

  object NoClassRepProvider extends ClassRepProvider {
    def classRep(name: String)      = NoClassRep
    def providesClass(name: String) = false
  }

  trait ClassRepProvider {
    /** The class representation for the given name, or NoClassRep
     *  if unknown.
     */
    def classRep(name: String): ClassRep

    /** Whether the given class name can be supplied if requested,
     *  assuming that is known.  Otherwise, false.
     */
    def providesClass(name: String): Boolean
  }

  trait ClassLoadingProvider extends ClassRepProvider {
    /** The ClassLoader used by this provider.
     */
    def classLoader: ClassLoader

    /** True if this provider can supply a class representation for
     *  the given name.
     */
    def classInstance(name: String): Class[_]
  }

  trait ClassDefiningProvider extends ClassLoadingProvider {
    /** Define a class based on the given name and bytecode.
     */
    def defineClass(name: String, bytes: Array[Byte]): Class[_]
  }

  trait EnumerableProvider extends ClassRepProvider {
    /** All the classes in this package.
     */
    def classes: Iterable[ClassRep]
  }

  trait PackageProvider extends EnumerableProvider {
    /** The name of this package.
     */
    def packageName: String

    /** The providers for this package's immediate subpackages.
     */
    def packages: Iterable[PackageProvider]
  }
}
