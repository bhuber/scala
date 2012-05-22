/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.runtime

final class RichLong(val self: Long) extends AnyVal {
  /**
    * @return `'''this'''` if `'''this''' < that` or `that` otherwise
    */
  def min(that: Long): Long = if (self < that) self else that

  /**
    * @return `'''this'''` if `'''this''' > that` or `that` otherwise
    */
  def max(that: Long): Long = if (self > that) self else that

  def toBinaryString: String = java.lang.Long.toBinaryString(self)
  def toHexString: String = java.lang.Long.toHexString(self)
  def toOctalString: String = java.lang.Long.toOctalString(self)
}
