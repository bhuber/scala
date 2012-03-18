package scala.io
package internal

import scala.io.ClassProvision._
import scala.tools.nsc.io.{ AbstractFile, VirtualFile, NoAbstractFile }

private abstract class CFile(f: AbstractFile, fileKind: String) extends ClassRepFile {
  def path          = f.path
  def lastModified  = f.lastModified
  def isEmpty       = false
  override def hashCode = path.##
  override def equals(other: Any) = other match {
    case x: ClassRepFile  => path == x.path
    case _                => super.equals(other)
  }
  override def toString = "<%s %s>".format(fileKind, path)
}
private class SFile(f: AbstractFile, extension: String) extends CFile(f, extension + " source") with ClassSourceFile {
  def name      = f.name
  def className = f.name stripSuffix ("." + extension)
  def chars     = f.toCharArray
  def bytes     = scala.io.Codec.toUTF8(chars, 0, chars.length)
}
private class BFile(f: AbstractFile) extends CFile(f, "class file") with ClassBinaryFile {
  require (f hasExtension "class", this)

  def name      = className
  def className = f.name stripSuffix ".class"
  def bytes     = f.toByteArray
}

object ClassSourceFile {
  private class ScalaFile(f0: AbstractFile) extends SFile(f0, "scala") { }
  private class JavaFile(f0: AbstractFile) extends SFile(f0, "java") { }

  def apply(f: AbstractFile): ClassSourceFile =
    if ((f eq null) || (f eq NoAbstractFile)) NoClassSourceFile
    else if (f hasExtension "scala") new ScalaFile(f)
    else if (f hasExtension "java") new JavaFile(f)
    else new SFile(f, "unknown")

  implicit def fromAbstractFile(f: AbstractFile): ClassSourceFile = apply(f)
}

object ClassBinaryFile {
  def apply(f: AbstractFile): ClassBinaryFile =
    if ((f eq null) || (f eq NoAbstractFile)) NoClassBinaryFile
    else new BFile(f)

  implicit def fromAbstractFile(f: AbstractFile): ClassBinaryFile = apply(f)
}
