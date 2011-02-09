/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package ast

import java.io.{ OutputStream, PrintWriter, StringWriter, Writer }
import symtab.Flags._
import symtab.SymbolTable
import scala.collection.{ mutable, immutable, generic }

trait TreePrintingTraversers {
  self: SymbolTable =>

  import treeInfo.{ IsTrue, IsFalse }

  /** A tree printer which is stingier about vertical whitespace and unnecessary
   *  punctuation than the standard one.
   */
  class CompactTreePrinter(out: PrintWriter) extends StdTreePrinter(out) {
    override def printRow(ts: List[Tree], start: String, sep: String, end: String) {
      print(start)
      printSeq(ts)(print)(print(sep))
      print(end)
    }

    // drill down through Blocks and pull out the real statements.
    def allStatements(t: Tree): List[Tree] = t match {
      case Block(stmts, expr) => (stmts flatMap allStatements) ::: List(expr)
      case _                  => List(t)
    }

    def printLogicalOr(t1: (Tree, Boolean), t2: (Tree, Boolean)) =
      printLogicalOp(t1, t2, "||")

    def printLogicalAnd(t1: (Tree, Boolean), t2: (Tree, Boolean)) =
      printLogicalOp(t1, t2, "&&")

    def printLogicalOp(t1: (Tree, Boolean), t2: (Tree, Boolean), op: String) = {
      def maybenot(tvalue: Boolean) = if (tvalue) "" else "!"

      print("%s(" format maybenot(t1._2))
      printRaw(t1._1)
      print(") %s %s(".format(op, maybenot(t2._2)))
      printRaw(t2._1)
      print(")")
    }

    override def printRaw(tree: Tree): Unit = {
      // routing supercalls through this for debugging ease
      def s() = super.printRaw(tree)

      tree match {
        // labels used for jumps - does not map to valid scala code
        case LabelDef(name, params, rhs) =>
          print("labeldef %s(%s) = ".format(name, params mkString ","))
          printRaw(rhs)

        case Ident(name) =>
          print(decodedSymName(tree, name))

        // target.method(arg) ==> target method arg
        case Apply(Select(target, method), List(arg)) =>
          if (method.decode.toString == "||")
            printLogicalOr(target -> true, arg -> true)
          else if (method.decode.toString == "&&")
            printLogicalAnd(target -> true, arg -> true)
          else (target, arg) match {
            case (_: Ident, _: Literal | _: Ident)  =>
              printRaw(target)
              print(" ")
              printRaw(Ident(method))
              print(" ")
              printRaw(arg)
            case _                        => s()
          }

        // target.unary_! ==> !target
        case Select(qualifier, name) if (name.decode startsWith "unary_") =>
          print(name.decode drop 6)
          printRaw(qualifier)

        case Select(qualifier, name) =>
          printRaw(qualifier)
          print(".")
          print(name.decode)

        // target.toString() ==> target.toString
        case Apply(fn, Nil)   => printRaw(fn)

        // if a Block only continues one actual statement, just print it.
        case Block(stats, expr) =>
          allStatements(tree) match {
            case List(x)            => printRaw(x)
            case xs                 => s()
          }

        // We get a lot of this stuff
        case If( IsTrue(), x, _)        => printRaw(x)
        case If(IsFalse(), _, x)        => printRaw(x)

        case If(cond,  IsTrue(), elsep)   =>  printLogicalOr(cond -> true, elsep -> true)
        case If(cond, IsFalse(), elsep)   => printLogicalAnd(cond -> false, elsep -> true)
        case If(cond,  thenp, IsTrue())   =>  printLogicalOr(cond -> false, thenp -> true)
        case If(cond,  thenp, IsFalse())  => printLogicalAnd(cond -> true, thenp -> true)

        // If thenp or elsep has only one statement, it doesn't need more than one line.
        case If(cond, thenp, elsep) =>
          def ifIndented(x: Tree) = {
            indent ; println() ; printRaw(x) ; undent
          }

          val List(thenStmts, elseStmts) = List(thenp, elsep) map allStatements
          print("if ("); print(cond); print(")")

          thenStmts match {
            case List(x: If)  => ifIndented(x)
            case List(x)      => printRaw(x)
            case _            => printRaw(thenp)
          }

          if (elseStmts.nonEmpty) {
            print("else")
            indent ; println()
            elseStmts match {
              case List(x)  => printRaw(x)
              case _        => printRaw(elsep)
            }
            undent ; println()
          }
        case _        => s()
      }
    }
  }

  /** This must guarantee not to force any evaluation, so we can learn
   *  a little bit about trees in the midst of compilation without altering
   *  the natural course of events.
   */
  class SafeTreePrinter(out: PrintWriter) extends StdTreePrinter(out) {
    override def print(tree: Tree) {
      printPosition(tree)
      printRaw(tree)
    }
    private def default(t: Tree) = t.getClass.getName.reverse.takeWhile(_ != '.').reverse
    private def params(trees: List[Tree]): String = trees map safe mkString ", "

    private def safe(name: Name): String = name.decode
    private def safe(tree: Tree): String = tree match {
      case Apply(fn, args)        => "%s(%s)".format(safe(fn), params(args))
      case Select(qual, name)     => safe(qual) + "." + safe(name)
      case This(qual)             => safe(qual) + ".this"
      case Ident(name)            => safe(name)
      case Literal(value)         => value.stringValue
      case _                      => "(?: %s)".format(default(tree))
    }

    override def printRaw(tree: Tree) { print(safe(tree)) }
  }

  class TreeMatchTemplate {
    // non-trees defined in Trees
    //
    // case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int)
    // case class Modifiers(flags: Long, privateWithin: Name, annotations: List[Tree], positions: Map[Long, Position])
    //
    def apply(t: Tree): Unit = t match {
      // eliminated by typer
      case Annotated(annot, arg)  =>
      case AssignOrNamedArg(lhs, rhs) =>
      case DocDef(comment, definition) =>
      case Import(expr, selectors) =>

      // eliminated by refchecks
      case ModuleDef(mods, name, impl) =>
      case TypeTreeWithDeferredRefCheck() =>

      // eliminated by erasure
      case TypeDef(mods, name, tparams, rhs) =>
      case Typed(expr, tpt) =>

      // eliminated by cleanup
      case ApplyDynamic(qual, args) =>

      // eliminated by explicitouter
      case Alternative(trees) =>
      case Bind(name, body) =>
      case CaseDef(pat, guard, body) =>
      case Star(elem) =>
      case UnApply(fun, args) =>

      // eliminated by lambdalift
      case Function(vparams, body) =>

      // eliminated by uncurry
      case AppliedTypeTree(tpt, args) =>
      case CompoundTypeTree(templ) =>
      case ExistentialTypeTree(tpt, whereClauses) =>
      case SelectFromTypeTree(qual, selector) =>
      case SingletonTypeTree(ref) =>
      case TypeBoundsTree(lo, hi) =>

      // survivors
      case Apply(fun, args) =>
      case ArrayValue(elemtpt, trees) =>
      case Assign(lhs, rhs) =>
      case Block(stats, expr) =>
      case ClassDef(mods, name, tparams, impl) =>
      case DefDef(mods, name, tparams, vparamss, tpt, rhs)  =>
      case EmptyTree =>
      case Ident(name) =>
      case If(cond, thenp, elsep) =>
      case LabelDef(name, params, rhs) =>
      case Literal(value) =>
      case Match(selector, cases) =>
      case New(tpt) =>
      case PackageDef(pid, stats) =>
      case Return(expr) =>
      case Select(qualifier, selector) =>
      case Super(qual, mix) =>
      case Template(parents, self, body) =>
      case This(qual) =>
      case Throw(expr) =>
      case Try(block, catches, finalizer) =>
      case TypeApply(fun, args) =>
      case TypeTree() =>
      case ValDef(mods, name, tpt, rhs) =>

      // missing from the Trees comment
      case Parens(args) =>                          // only used during parsing
      case SelectFromArray(qual, name, erasure) =>  // only used during erasure
    }
  }
}