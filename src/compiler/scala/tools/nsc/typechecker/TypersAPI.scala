/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import util.TreeTracer
import scala.collection.{ mutable, immutable }

class IdentityHashSet {
  private[this] val map = new java.util.IdentityHashMap[AnyRef, AnyRef]
  
  def -=(x: AnyRef): this.type = { map.put(x, x); this }
  def +=(x: AnyRef): this.type = { map.remove(x); this }
  def apply(x: AnyRef): Boolean = { map containsKey x }
}

/** It's called "TypersAPI" but at this point it's not an API
 *  or even a proposed API
 */
trait TypersAPI {
  self: Analyzer =>

  import global._

  def newTyper(context: Context): Typer
  def finishTyper(): Unit

  trait TyperAPI {
    def typed(tree: Tree, mode: Int, pt: Type): Tree
    def typedClassDef(cdef: ClassDef): ClassDef
    def typedModuleDef(mdef: ModuleDef): ModuleDef
    def typedDefDef(ddef: DefDef): DefDef
    def typedTypeDef(tdef: TypeDef): TypeDef
    def typedValDef(vdef: ValDef): ValDef
    def typedTemplate(templ: Template, parents1: List[Tree]): Template
    // def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree]
    def typedStat(stat: Tree, exprOwner: Symbol, localTarget: Boolean): Tree
  }
}

trait AnalyzerAPI extends TypersAPI {
  self: Analyzer =>

  import global._
  
  def newNamer(context: Context): Namer
}

trait ProfilingAnalyzer extends Analyzer with AnalyzerAPI {
  import global._
  
  override def newNamer(context: Context): Namer = new Namer(context) with ProfilingNamer
  override def newTyper(context: Context): Typer = new Typer(context) with ProfilingTyper
  override def finishTyper(): Unit               = tracer.show()

  trait ProfilingTyper extends TyperAPI {
    abstract override def typed(tree: Tree, mode: Int, pt: Type): Tree =
      typing(tree)(super.typed(tree, mode, pt))
    abstract override def typedStat(stat: Tree, exprOwner: Symbol, localTarget: Boolean): Tree =
      typing(stat)(super.typedStat(stat, exprOwner, localTarget))
    abstract override def typedClassDef(cdef: ClassDef): ClassDef = 
      typing(cdef)(super.typedClassDef(cdef))
    abstract override def typedModuleDef(mdef: ModuleDef): ModuleDef =
      typing(mdef)(super.typedModuleDef(mdef))
    abstract override def typedDefDef(ddef: DefDef): DefDef =
      typing(ddef)(super.typedDefDef(ddef))
    abstract override def typedTypeDef(tdef: TypeDef): TypeDef =
      typing(tdef)(super.typedTypeDef(tdef))
    abstract override def typedValDef(vdef: ValDef): ValDef =
      typing(vdef)(super.typedValDef(vdef))
    abstract override def typedTemplate(templ: Template, parents1: List[Tree]): Template =
      typing(templ)(super.typedTemplate(templ, parents1))
  }
    
  trait ProfilingNamer extends Namer {
    override def enterSym(tree: Tree): Context = {
      typing(tree)(super.enterSym(tree))
    }
  }
  
  private val openTrees = new IdentityHashSet
  private def typing[T](tree: Tree)(body: => T): T =
    if (openTrees(tree)) body
    else {
      openTrees += tree
      try tracer.node(tree)(body)
      finally openTrees -= tree
    }
  
  private object tracer extends TreeTracer {
    type Traced = Tree
    type Pos = Position
    def positionOfTarget(target: Tree): Position = target.pos
    
    val MaxPos = 30
    val MillisThreshold = settings.warnSlowTyping.value

    if (settings.warnSlowTyping.isSetByUser)
      ProfilingAnalyzer.treeTracers ::= this
    
    def atTyper[T](body: => T): T = atPhase(currentRun.typerPhase)(body)
    
    def stringifyType(t: Tree) = {
      val tpe = t.tpe
      if (tpe == null || tpe == NoType) t.summaryString
      else "" + tpe
    }
    def stringifySymbol(t: Tree) = {
      val sym = t.symbol
      if (sym == null || sym == NoSymbol) t.summaryString
      else if (sym.isMethod) atTyper(sym.defString)
      else atTyper(sym.kindString + " " + sym.decodedName)
    }
    def positionContent(pos: Position): String = {
      if (pos.isRange) (
        pos.source.content
          slice (pos.start, pos.end)
          takeWhile (_ != '\n')
          mkString ""
      ).trim.replaceAll("""[={\s]+$""", "")
      else
        pos.toSingleLine.lineContent
    }
    private def hasEligibleSymbol(t: Tree) = {
      val sym = t.symbol
      (    (sym != null) 
        && (sym != NoSymbol) 
        && !sym.isAnonOrRefinementClass 
        && !sym.isSynthetic
        && !sym.isConstructor
      )
    }

    def shouldRecordTarget(recv: Tree) = recv match {
      case x: MemberDef           => true // hasEligibleSymbol(x)
      case x: ApplyToImplicitArgs => false
      case x: Apply               => true
      case _                      => false
    }
    def shouldDisplayNode(node: Node) = node.target match {
      case _: MemberDef | _: Apply => node.millis >= MillisThreshold
      case _                       => false
    }

    def stringifyArgs(args: Any*): String = args map stringify mkString ", "

    def stringifyTypTree(t: Tree): String = t match {
      case AppliedTypeTree(tpt, args)             => stringifyType(tpt) + "[" + stringifyArgs(args: _*) + "]"
      case SingletonTypeTree(ref)                 => stringify(ref)
      case SelectFromTypeTree(qual, name)         => stringifyType(qual) + "." + name
      case CompoundTypeTree(templ)                => t.symbol.info.parents mkString " with "
      case TypeBoundsTree(lo, hi)                 => stringifyType(t)
      case ExistentialTypeTree(tpt, whereClauses) => stringifyType(t)
      case _                                      => stringifyType(t)
    }
    
    def stringifyTree(t: Tree): String = t match {
      case x: DefDef                        => "def " + x.name
      case x: ValDef if x.mods.isMutable    => "var " + x.name
      case x: ValDef                        => "val " + x.name
      case x: ClassDef                      => "class " + x.name
      case x: ModuleDef                     => "object " + x.name
      case x: TypeDef                       => "type " + x.name
      case x: PackageDef                    => "package " + x.name
      case Select(qual, name)               => stringifyTree(qual) + "." + name
      case Apply(fn, args)                  => stringifySymbol(fn) + "(" + stringifyArgs(args: _*) + ")"
      case TypeApply(fun, args)             => stringifySymbol(fun) + "[" + stringifyArgs(args: _*) + "]"
      case Literal(lit)                     => "" + lit
      case _                                => t.summaryString
    }

    override def stringify(value: Any): String = value match {
      case x: ValOrDefDef                   => stringifySymbol(x)
      case x: Tree                          => positionContent(x.pos)
      // case x: Tree                          => stringifyTree(x)
      // case x: TypTree                    => stringifyTypTree(x)
      // case x: Tree                       => stringifyPos(x.pos)
      case xs: Traversable[_] if xs.isEmpty => "Nil"
      case x: Position                      => x.source.file.name + ":" + x.line //"line " + x.line
      case _                                => super.stringify(value)
    }

    override def toString = "tracer(threshold=% millis)".format(MillisThreshold)
  }
}

object ProfilingAnalyzer {
  private var treeTracers: List[TreeTracer] = Nil

  scala.sys addShutdownHook {
    treeTracers.reverse foreach (_ show)
  }
}


/** Other typer methods

    def typed(tree: Tree, mode: Int, pt: Type): Tree
    def typedArgs(args: List[Tree], mode: Int): List[Tree]
    def typedBlock(block: Block, mode: Int, pt: Type): Block
    def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree]
    def typedApply(fun: Tree, args: List[Tree]): Tree

    def typedAnnotated(ann: Tree, arg1: Tree): Tree
    def typedAnnotation(ann: Tree, mode: Int, selfsym: Symbol, annClass: Symbol, requireJava: Boolean): AnnotationInfo
    def typedAppliedTypeTree(tpt: Tree, args: List[Tree]): Tree
    def typedArg(arg: Tree, mode: Int, newmode: Int, pt: Type): Tree
    def typedArrayValue(elemtpt: Tree, elems: List[Tree]): Tree
    def typedAssign(lhs: Tree, rhs: Tree): Tree
    def typedBind(name: Name, body: Tree): Tree
    def typedBlock(block: Block, mode: Int, pt: Type): Block
    def typedCase(cdef: CaseDef, pattpe: Type, pt: Type): CaseDef
    def typedCases(tree: Tree, cases: List[CaseDef], pattp: Type, pt: Type): List[CaseDef]
    def typedCompoundTypeTree(templ: Template): Tree
    def typedEta(expr1: Tree): Tree
    def typedFunction(fun: Function, mode: Int, pt: Type): Tree
    def typedHigherKindedType(tree: Tree, mode: Int, pt: Type): Tree
    def typedIdent(name: Name): Tree
    def typedIf(cond: Tree, thenp: Tree, elsep: Tree): Tree
    def typedImport(imp: Import): Import
    def typedLabelDef(ldef: LabelDef): LabelDef
    def typedMatch(tree: Tree, selector: Tree, cases: List[CaseDef]): Tree
    def typedNew(tpt: Tree): Tree
    def typedOperator(tree: Tree): Tree
    def typedPattern(tree: Tree, pt: Type): Tree
    def typedPos(pos: Position)(tree: Tree): Tree
    def typedQualifier(tree: Tree, mode: Int, pt: Type): Tree
    def typedRefinement(stats: List[Tree]): Unit  // !!!
    def typedReturn(expr: Tree): Tree
    def typedSelect(qual: Tree, name: Name): Tree
    def typedStat(stat: Tree): Tree
    def typedSuper(qual: Tree, mix: TypeName): Tree
    def typedTemplate(templ: Template, parents1: List[Tree]): Template
    def typedThis(qual: Name): Tree
    def typedType(tree: Tree): Tree
    def typedType(tree: Tree, mode: Int): Tree
    def typedTypeConstructor(tree: Tree, mode: Int): Tree
    def typedUseCase(useCase: UseCase): Unit  // !!!

**/
