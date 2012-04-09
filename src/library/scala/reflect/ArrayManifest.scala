/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2007-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.reflect

/** An interface for array creation.  Although ClassManifest[T]
 *  is the usual way to provide ArrayManifest[T], this is as much
 *  as is necessary to inform the compiler that Arrays of element
 *  type T can be created.
 *
 *  TODO - further separate this so def newArray can be impelmented
 *  and taken advantage of without mandating the multidimensional methods.
 */
trait ArrayManifest[T] {
  def newArray(len: Int): Array[T]
  def newArray2(len: Int): Array[Array[T]]
  def newArray3(len: Int): Array[Array[Array[T]]]
  def newArray4(len: Int): Array[Array[Array[Array[T]]]]
  def newArray5(len: Int): Array[Array[Array[Array[Array[T]]]]]
}
