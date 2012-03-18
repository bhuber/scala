/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import io.AbstractFile
import ClassPath._
import scala.io.ClassProvision._

object ClassPathPrinter {
  // pretty print the contents of the classpath
  def show(cp: ClassPath): Unit = new ClassPathPrinter(cp).show()
  def show(cp: ClassPath, filter: Filter): Unit =
    new ClassPathPrinter(cp, filter).show()

  case class Filter(
    cpCondition: ClassPath => Boolean,
    classCondition: ClassRep => Boolean
  )
  object Filter {
    def empty = Filter(_ => true, _ => true)

    implicit def fromCpCondition(f: ClassPath => Boolean): Filter = Filter(f, _ => true)
    implicit def fromClassCondition(f: ClassRep => Boolean): Filter = Filter(_ => true, f)
  }
}
import ClassPathPrinter._

private class ClassPathPrinter(
  root: ClassPath,
  filter: Filter   = Filter.empty,
  indent: Indenter = (new util.Indenter).sorted
) {
  import indent._
  val Filter(cpCondition, classCondition) = filter

  def show() = dump(root)
  def allPackages(): Iterable[ClassPath] = allPackages(root)
  def allPackagesWithNames(): Iterable[(String, ClassPath)] = allPackagesWithNames(root)
  def allPackageNames(): Iterable[String] = {
    def subpackages(prefix: String, cp: ClassPath): Seq[String] = Nil ++ (
      cp.packages.map(prefix + _.name) ++
      cp.packages.flatMap(x => subpackages(prefix + x.name + ".", x))
    )
    subpackages("", root)
  }

  private def isAnon(x: Any) = {
    val name = "" + x
    (name contains "$anon") || {
      val (a, b) = name.reverse span (_.isDigit)
      a.nonEmpty && (b.head == '$')
    }
  }
  private def isSpec(x: Any) = ("" + x) contains "$sp"
  private def dumpClasses(classes: Iterable[ClassRep]) {
    val classNames = classes filter classCondition map ("" + _)
    if (classNames.nonEmpty) {
      val (anon, ok1) = classNames partition isAnon
      val (spec, ok2) = ok1 partition isSpec

      if (anon.nonEmpty)
        pp("// Omitting " + anon.size + " anonymous classes")
      if (spec.nonEmpty)
        pp("// Omitting " + spec.size + " specialized classes")

      pps(ok2)
    }
  }
  private def dumpDirectory(cp: DirectoryClassPath): Unit = {
    import cp.{ dir, name }

    val summary = List(
      if (cp.packages.isEmpty) "" else cp.packages.size + " packages" ,
      if (cp.classes.isEmpty) "" else cp.classes.size + " classes"
    ).filterNot(_ == "").mkString(", ")

    if (summary == "")
      pp("package " + name + " { }")
    else {
      val desc = (
        if (dir hasExtension "jar") dir.path
        else "package " + name + " {    // " + summary
      )
      block(desc) {
        dumpClasses(cp.classes)
        cp.packages foreach dump
      }
      pp("} // " + name)
    }
  }

  /** Information which entails walking the tree.  This is probably only
   *  necessary for tracking down problems - it's normally not used.
   */
  private def allPackages(cp: ClassPath): Iterable[ClassPath] =
    cp.packages ++ cp.packages.flatMap(allPackages)

  private def allPackagesWithNames(cp: ClassPath): Iterable[(String, ClassPath)] = {
    val paths = cp.packages map (p => p.name -> p)
    val subs =
      for ((prefix, p) <- paths ; (k, v) <- allPackagesWithNames(p)) yield
        (prefix + "." + k, v)

    paths ++ subs
  }

  // Dump a classpath.
  private def dump(cp: ClassPath): Unit = cp match {
    case x: MergedClassPath =>
      x.entries foreach dump

    case x: DirectoryClassPath =>
      if (cpCondition(x)) {
        dumpDirectory(x)
        block(x.asClasspathString) {
          x.sourcePathFiles foreach pp
        }
        pp("")
      }
  }
}