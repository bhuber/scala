package scala.tools
package api

import scala.tools.nsc.Global

trait Typers {
  val global: Global

  import global._
  import analyzer.Context

  def newTyper(context: Context): Typer

  trait Typer {
    def typedArgs(args0: List[Tree], mode: Int, formals0: List[Type], adapted0: List[Type]): List[Tree]
    def typedArgs(args: List[Tree], mode: Int): List[Tree]
    def typedCases(tree: Tree, cases: List[CaseDef], pattp: Type, pt: Type): List[CaseDef]
    def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree]

    def typed(tree: Tree, mode: Int, pt: Type): Tree
    def typedAnnotated(ann: Tree, arg1: Tree): Tree
    def typedAnnotation(ann: Tree, mode: Int, selfsym: Symbol, annClass: Symbol, requireJava: Boolean): AnnotationInfo
    def typedAppliedTypeTree(tpt: Tree, args: List[Tree]): Tree
    def typedApply(fun: Tree, args: List[Tree]): Tree
    def typedArg(arg: Tree, mode: Int, newmode: Int, pt: Type): Tree
    def typedArrayValue(elemtpt: Tree, elems: List[Tree]): Tree
    def typedAssign(lhs: Tree, rhs: Tree): Tree
    def typedBind(name: Name, body: Tree): Tree
    def typedBlock(block: Block, mode: Int, pt: Type): Block
    def typedCase(cdef: CaseDef, pattpe: Type, pt: Type): CaseDef
    def typedClassDef(cdef: ClassDef): Tree
    def typedCompoundTypeTree(templ: Template): Tree
    def typedDefDef(ddef: DefDef): DefDef
    def typedEta(expr1: Tree): Tree
    def typedFunction(fun: Function, mode: Int, pt: Type): Tree
    def typedHigherKindedType(tree: Tree, mode: Int, pt: Type): Tree
    def typedIdent(name: Name): Tree
    def typedIf(cond: Tree, thenp: Tree, elsep: Tree): Tree
    def typedImport(imp: Import): Import
    def typedLabelDef(ldef: LabelDef): LabelDef
    def typedMatch(tree: Tree, selector: Tree, cases: List[CaseDef]): Tree
    def typedModuleDef(mdef: ModuleDef): Tree
    def typedNew(tpt: Tree): Tree
    def typedOperator(tree: Tree): Tree
    def typedPattern(tree: Tree, pt: Type): Tree
    def typedPos(pos: Position)(tree: Tree): Tree
    def typedQualifier(tree: Tree, mode: Int, pt: Type): Tree
    def typedRefinement(stats: List[Tree]): Tree
    def typedReturn(expr: Tree): Tree
    def typedSelect(qual: Tree, name: Name): Tree
    def typedStat(stat: Tree): Tree
    def typedSuper(qual: Tree, mix: TypeName): Tree
    def typedTemplate(templ: Template, parents1: List[Tree]): Template
    def typedThis(qual: Name): Tree
    def typedType(tree: Tree): Tree
    def typedType(tree: Tree, mode: Int): Tree
    def typedTypeConstructor(tree: Tree, mode: Int): Tree
    def typedTypeDef(tdef: TypeDef): TypeDef
    def typedUseCase(useCase: UseCase): Unit  // !!!
    def typedValDef(vdef: ValDef): ValDef
  }
}
