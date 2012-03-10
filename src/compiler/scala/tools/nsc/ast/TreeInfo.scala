/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package ast

import reflect.internal.Flags._
import symtab._

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
abstract class TreeInfo extends reflect.internal.TreeInfo {
  val global: Global
  import global._

  /** The source code corresponding to the given tree, as
   *  derived from the range position's mapping into the source
   *  file, and adjusting for the fact that xml positions are
   *  broken but when I fixed them it broke the IDE.
   */
  def sourceCode(tree: Tree) = {
    if (isXmlLiteral(expressionWithoutBlocks(tree)))
      "<" + tree.pos.sourceCode
    else
      tree.pos.sourceCode
  }
  
  def expressionWithoutBlocks(tree: Tree): Tree = tree match {
    case Block(Nil, expr) => expressionWithoutBlocks(expr)
    case _                => tree
  }

  /** Is tree legal as a member definition of an interface?
   */
  override def isInterfaceMember(tree: Tree): Boolean = tree match {
    case DocDef(_, definition)         => isInterfaceMember(definition)
    case _ => super.isInterfaceMember(tree)
  }

  /** Is tree a pure (i.e. non-side-effecting) definition?
   */
  override def isPureDef(tree: Tree): Boolean = tree match {
    case DocDef(_, definition) => isPureDef(definition)
    case _ => super.isPureDef(tree)
  }

 /** Does list of trees start with a definition of
   *  a class of module with given name (ignoring imports)
   */
  override def firstDefinesClassOrObject(trees: List[Tree], name: Name): Boolean = trees match {
    case ClassDef(_, `name`, _, _) :: Nil => true
    case _ => super.firstDefinesClassOrObject(trees, name)
  }
}
