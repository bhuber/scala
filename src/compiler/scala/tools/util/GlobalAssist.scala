package scala.tools
package util

import java.io.{ IOException, File => JFile }
import java.net.URL
import scala.tools.nsc.{ Phase, Global, Settings }
import scala.tools.nsc.io.{ Path, PlainFile, AbstractFile, Streamable }
import scala.reflect.internal.util.{ SourceFile, BatchSourceFile }
import scala.collection.{ mutable, immutable }

// A convenient companion for when you don't care about the
// global so much as the run
object GlobalAssist extends GlobalAssist(new Global(new Settings())) { }

// See test/files/run/global-assist.scala examples.
class GlobalAssist[T <: Global](val global: T) {
  import global._

  def stopAfterPhase = "typer"

  def fromUrls(urls: List[URL]): TypingRun              = fromSources(urls map mkSource)
  def fromAbFiles(files: List[AbstractFile]): TypingRun = fromSources(files map mkSource)
  def fromFiles(files: List[JFile]): TypingRun          = fromSources(files map mkSource)
  def fromSources(files: List[SourceFile]): TypingRun   = TypingRun(files)

  def apply(f: JFile, files: JFile*): TypingRun               = fromFiles(f :: files.toList)
  def apply(f: AbstractFile, files: AbstractFile*): TypingRun = fromAbFiles(f :: files.toList)
  def apply(f: URL, files: URL*): TypingRun                   = fromUrls(f :: files.toList)
  def apply(f: SourceFile, files: SourceFile*): TypingRun     = fromSources(f :: files.toList)

  private def mkSource(file: JFile): SourceFile        = mkSource(new PlainFile(file))
  private def mkSource(url: URL): SourceFile           = newSourceFile(Streamable slurp url)
  private def mkSource(file: AbstractFile): SourceFile = new BatchSourceFile(file)

  private def mkSources(files: List[JFile]) = files filter (_.getName endsWith ".scala") map mkSource

  // Disambiguating String intentions via uniqely named methods
  def codes(xs: String*): TypingRun = fromSources(xs.toList map newSourceFile)
  def paths(xs: String*): TypingRun = fromFiles(xs.toList map (x => new JFile(x)))
  def urls(xs: String*): TypingRun  = fromUrls(xs.toList map (x => new URL(x)))
  def guess(xs: String*): TypingRun = fromSources(xs.toList flatMap { x =>
    try List(mkSource(new URL(x)))                            // maybe url
    catch { case _: IOException =>
      val f = new JFile(x)
      if (f.isFile) List(mkSource(f))                         // maybe file
      else if (f.isDirectory) mkSources(f.listFiles.toList)   // maybe dir
      else List(newSourceFile(x))                             // maybe literal code
    }
  })

  // Save/restore settings.
  def stoppingAfter[T](ph: String)(body: => T): T = {
    val saved = settings.stopAfter.value
    settings.stopAfter.value = List(ph)
    try body
    finally settings.stopAfter.value = saved
  }
  object TypingRun {
    def apply(sources: List[SourceFile]): TypingRun = {
      stoppingAfter(stopAfterPhase) {
        val run = new TypingRun(sources)
        run.success
        run
      }
    }
  }
  class TypingRun private (val files: List[SourceFile]) extends Run {
    final val global: GlobalAssist.this.global.type = GlobalAssist.this.global
    override def progress(current: Int, total: Int) {
      // println("progress" + ((current, total)))
    }
    override def informUnitStarting(phase: Phase, unit: CompilationUnit) {
      // println("informUnitStarting" + ((phase, unit)))
    }

    lazy val trees   = if (success) units.toList map (_.body) else Nil
    lazy val success = {
      reporter.reset
      compileSources(files)
      !reporter.hasErrors
    }
    // A single-tree result if desired.
    def tree = trees match {
      case Nil      => EmptyTree
      case x :: Nil => x
      case _        => Block(trees.init, trees.last)
    }

    /** Avoiding ValDefs which aren't actually val defs.
     *  That is, parameters and self-types.
     */
    class ValOrDefDefExtractor extends Traverser {
      val buf = mutable.ListBuffer[ValOrDefDef]()

      override def traverse(t: Tree) = t match {
        case Template(_, _, body) =>
          traverseStats(body, t.symbol)
        case defn: ValOrDefDef =>
          buf += defn
          super.traverse(defn.rhs)
        case _ =>
          super.traverse(t)
      }
    }
    private def valOrDefDefsIn(t: Tree): List[ValOrDefDef] = {
      val traverser = new ValOrDefDefExtractor
      traverser traverse t
      traverser.buf.toList
    }

    // Trees
    def classDefs    = implDefs collect { case t: ClassDef => t }
    def defTrees     = flatTrees collect { case t: DefTree => t }
    def flatTrees    = trees flatMap (_ collect { case t => t })
    def idents       = refTrees collect { case t: Ident => t }
    def implDefs     = flatTrees collect { case t: ImplDef => t }
    def moduleDefs   = implDefs collect { case t: ModuleDef => t }
    def refTrees     = flatTrees collect { case t: RefTree => t }
    def typeDefs     = flatTrees collect { case t: TypeDef => t }
    def valOrDefDefs = trees flatMap valOrDefDefsIn

    // Symbols
    def allSymbols       = flatTrees map (_.symbol) filterNot (_ eq null) distinct
    def classes          = classDefs map (_.symbol)
    def definitions      = afterTyper(defTrees map (_.symbol.initialize))
    def methods          = valOrDefDefs map (_.symbol) filter (_.isMethod)
    def moduleClasses    = modules map (_.moduleClass)
    def modules          = moduleDefs map (_.symbol)
    def publicReferences = references filterNot (sym => sym.isPrivate || sym.isLocal)
    def references       = afterTyper(refTrees map (_.symbol.initialize) filterNot (_ eq NoSymbol) distinct)
    def topLevel         = classes ++ modules filter (_.isDefinedInPackage)

    // Types
    def allTypes      = flatTrees map (_.tpe) filterNot (_ eq null) distinct
    def constantTypes = allTypes collect { case t: ConstantType => t }
    def polyTypes     = allTypes collect { case t: PolyType => t }
  }
}
