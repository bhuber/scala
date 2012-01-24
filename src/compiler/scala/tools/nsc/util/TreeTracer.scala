/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package util

import scala.collection.{ mutable, immutable }
import mutable.ListBuffer

/** A generic infrastructure for tracing recursive data structures.
 *  Specific applications include tracing the method call chain through
 *  a set of methods of interest and tracing the typing of trees.
 *  All the output can be delayed until shutdown so that one can
 *  trace sensitive structures without disturbing them, as toString
 *  still tends to force lazy data at times when one would wish otherwise.
 */
abstract class TreeTracer { tracer =>

  type Traced <: AnyRef
  type Pos <: AnyRef
  
  def positionOfTarget(target: Traced): Pos
  def shouldRecordTarget(target: Traced): Boolean
  def shouldDisplayNode(node: Node): Boolean

  def stringify(value: Any): String = {
    try "" + value 
    catch { case x => "" + x }
  }
  def stringifyResult(result: Any): String = ""
  def quieter(s: String) = List(
    """\Qscala.collection.\E""" -> "sc."
  ).foldLeft(s) { case (s, (from, to)) => s.replaceAll(from, to) }
  
  def truncate(max: Int)(s: String): String = 
    if (s.length < max) s 
    else s.substring(max - 6) + " [...]"
  
  def lineFormatString = "%-20s %-50s %s"

  def showNode(node: Node, depth: Int) {
    def showKids() = node.children foreach (n => showNode(n, depth + 1))
    if (shouldDisplayNode(node)) {
      val linedIndent  = "| " * depth
      val str = lineFormatString.format(
        node.timeString + linedIndent + node.id,
        truncate(50)(stringify(node.target)).trim,
        stringify(positionOfTarget(node.target))
      )
      println(quieter(str))
      if (node.children.nonEmpty) {
        showKids()
        println((" " * node.timeString.length) + linedIndent + """\--""" + node.id)
      }
    }
    else showKids()
  }
  
  def show() {
    openChildren(0) foreach (n => showNode(n, 0))
  }

  // private val rootNodes         = ListBuffer[Node]()
  // private val openChildren      = ListBuffer(ListBuffer[Node]())
  private val openChildren      = mutable.HashMap[Int, List[Node]]() withDefaultValue Nil
  private val openTargets       = ListBuffer[Traced]()
  private val finished          = mutable.HashSet[Int]()        // completed targets
  private var nextId            = 0                             // index of the next new target
  private var oldestOpenId      = 0                             // index of the oldest open target
  private var nodes: List[Node] = Nil                           // list of all targets
  
  private def getIdAndIncrement()   = 
    try nextId finally nextId += 1

  private def clearChildrenAtDepth(n: Int) =
    try openChildren(n).reverse
    finally openChildren -= n
  
  private def pushTarget[T](target: Traced)(body: => T): T = {
    openTargets += target
    openChildren(openTargets.size) = Nil
    
    try body
    finally openTargets trimEnd 1
  }

  /** Enters a new node.
   */
  def node[T](target: Traced)(body: => T): T = {
    if (!shouldRecordTarget(target) || (openTargets contains target))
      return body

    val depth      = openTargets.size
    val childDepth = depth + 1
    val id         = getIdAndIncrement()
    
    pushTarget(target) {
      val startTime = System.nanoTime
      val result    = body
      val nanos     = System.nanoTime - startTime
      val children  = clearChildrenAtDepth(childDepth)
      val node      = new Node(id, nextId, nanos, target, result, children)

      openChildren(depth) ::= node
      result
    }
  }
  
  class Node(
    val id: Int,
    val endRange: Int,
    val nanos: Long,
    val target: Traced,     // target value (incoming data)
    val result: Any,        // result value (outgoing data)
    val children: Seq[Node] // immediate children
  ) {
    // var depth: Int = 0
    
    def millis       = nanos / 1000000L
    def ownMillis    = (nanos - children.map(_.nanos).sum) / 1000000L
    def timeString   = {
      if (millis == 0) " " * 11
      else if (millis == ownMillis) "%5s/     " format millis
      else "%5s/%-5s".format(millis, ownMillis)
    }
    // def targetString = stringify(target)
    // def resultString = stringifyResult(result)
    // def resultString   = stringify(result) match { case "" => "" ; case s => " --> " + s }
    // 
    // def isSimple = width == 1
    // def width    = out - in
    // def indent   = "| " * depth
    // 
    // def inIndent  = timeString + indent
    // def outIndent = (" " * timeString.length) + indent
    // def inString = {
    //   val s = inIndent + id
    //   val sp = " " * (32 - s.length)
    //   s + sp + targetString + resultString
    // }
    // def outString = outIndent + """\--""" + id

    override def equals(other: Any) = other match {
      case x: Node  => id == x.id
      case _        => false
    }
    override def hashCode = id
  }
}
