/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import util.Statistics._

/** The main attribution phase.
 */
trait Analyzer extends AnyRef
            with Contexts
            with Namers
            with Typers
            with Infer
            with Implicits
            with Variances
            with EtaExpansion
            with SyntheticMethods
            with Unapplies
            with Macros
            with NamesDefaults
            with TypeDiagnostics
            with ContextErrors
            with StdAttachments
{
  val global: Global
  import global._

  def isAsSpecific(infer: Inferencer, ftpe1: Type, ftpe2: Type): Boolean =
    infer.isAsSpecificInternal(ftpe1, ftpe2)

  def isAsSpecificValueType(infer: Inferencer, tpe1: Type, tpe2: Type, undef1: List[Symbol], undef2: List[Symbol]): Boolean =
    infer.isAsSpecificValueTypeInternal(tpe1, tpe2, undef1, undef2)

  def isStrictlyMoreSpecific(infer: Inferencer, ftpe1: Type, ftpe2: Type, sym1: Symbol, sym2: Symbol): Boolean =
    infer.isStrictlyMoreSpecificInternal(ftpe1, ftpe2, sym1, sym2)

  private def globalSingleton: Analyzer.this.global.type = global

  object namerFactory extends SubComponent {
    val global = globalSingleton
    val phaseName = "namer"
    val runsAfter = List[String]("parser")
    val runsRightAfter = None
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      override def keepsTypeParams = false

      def apply(unit: CompilationUnit) {
        newNamer(rootContext(unit)).enterSym(unit.body)
      }
    }
  }

  object packageObjects extends SubComponent {
    val global = globalSingleton
    val phaseName = "packageobjects"
    val runsAfter = List[String]()
    val runsRightAfter= Some("namer")

    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override val checkable = false
      import global._

      val openPackageObjectsTraverser = new Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case ModuleDef(_, _, _) =>
            if (tree.symbol.name == nme.PACKAGEkw) {
              openPackageModule(tree.symbol, tree.symbol.owner)
            }
          case ClassDef(_, _, _, _) => () // make it fast
          case _ => super.traverse(tree)
        }
      }

      def apply(unit: CompilationUnit) {
        openPackageObjectsTraverser(unit.body)
      }
    }
  }

  object typerFactory extends SubComponent {
    val global = globalSingleton
    val phaseName = "typer"
    val runsAfter = List[String]()
    val runsRightAfter = Some("packageobjects")
    def newPhase(_prev: Phase): StdPhase = new StdPhase(_prev) {
      override def keepsTypeParams = false
      resetTyper()
      // the log accumulates entries over time, even though it should not (Adriaan, Martin said so).
      // Lacking a better fix, we clear it here (before the phase is created, meaning for each
      // compiler run). This is good enough for the resident compiler, which was the most affected.
      undoLog.clear()
      override def run() {
        val start = startTimer(typerNanos)
        global.echoPhaseSummary(this)
        currentRun.units foreach applyPhase
        undoLog.clear()
        // need to clear it after as well or 10K+ accumulated entries are
        // uncollectable the rest of the way.
        stopTimer(typerNanos, start)
      }
      def apply(unit: CompilationUnit) {
        try {
          unit.body = newTyper(rootContext(unit)).typed(unit.body)
          if (global.settings.Yrangepos.value && !global.reporter.hasErrors) global.validatePositions(unit.body)
          for (workItem <- unit.toCheck) workItem()
        } finally {
          unit.toCheck.clear()
        }
      }
    }
  }
}

