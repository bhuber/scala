/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package util

import java.net.URL
import io._
import ClassPath.Logic
import JavaClassPath._
import scala.io.{ ClassProvision => CP }

/**
 * The classpath when compiling with target:jvm. Binary files (classfiles) are represented
 * as AbstractFile. nsc.io.ZipArchive is used to view zip/jar archives as directories.
 */
class JavaClassPath(entries: IndexedSeq[ClassPath], val logic: Logic) extends MergedClassPath(entries) {

}

object JavaClassPath {
  import ClassPath._

  object DefaultLogic extends Logic { }

  def fromURLs(urls: Seq[URL], javaLogic: Logic): JavaClassPath = {
    val entries = {
      for (url <- urls ; f = AbstractFile getURL url ; if f != null) yield
        javaLogic.newClassPath(f)
    }
    new JavaClassPath(entries.toIndexedSeq, javaLogic)
  }
  def fromURLs(urls: Seq[URL]): JavaClassPath =
    fromURLs(urls, DefaultLogic)

  def fromClasspath(cp: String): JavaClassPath =
    fromClasspath(cp, DefaultLogic)

  def fromClasspath(cp: String, javaLogic: Logic): JavaClassPath =
    new JavaClassPath(javaLogic.classesInExpandedPath(cp), javaLogic)
}
