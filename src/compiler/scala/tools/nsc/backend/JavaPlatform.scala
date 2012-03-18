/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import scala.tools.util.PathResolver
import scala.io.ClassProvision._
import scala.tools.nsc.util.CompilerClassProvider

trait JavaPlatform extends Platform {
  import global._
  import definitions._

  lazy val classProvider: CompilerClassProvider = new PathResolver(settings).result
  def rootLoader = new loaders.PackageLoader(classProvider)

  // [Martin] Why do we need a cast here?
  // The problem is that we cannot specify at this point that global.platform should be of type JavaPlatform.
  // So we cannot infer that global.platform.BinaryRepr is AbstractFile.
  // Ideally, we should be able to write at the top of the JavaPlatform trait:
  //   val global: Global { val platform: JavaPlatform }
  //   import global._
  // Right now, this does nothing because the concrete definition of platform in Global
  // replaces the tighter abstract definition here. If we had DOT typing rules, the two
  // types would be conjoined and everything would work out. Yet another reason to push for DOT.

  private def depAnalysisPhase =
    if (settings.make.isDefault) Nil
    else List(dependencyAnalysis)

  def platformPhases = List(
    flatten,    // get rid of inner classes
    genJVM      // generate .class files
  ) ++ depAnalysisPhase

  lazy val externalEquals          = getMember(BoxesRunTimeClass, nme.equals_)
  lazy val externalEqualsNumNum    = getMember(BoxesRunTimeClass, nme.equalsNumNum)
  lazy val externalEqualsNumChar   = getMember(BoxesRunTimeClass, nme.equalsNumChar)
  lazy val externalEqualsNumObject = getMember(BoxesRunTimeClass, nme.equalsNumObject)

  /** We could get away with excluding BoxedBooleanClass for the
   *  purpose of equality testing since it need not compare equal
   *  to anything but other booleans, but it should be present in
   *  case this is put to other uses.
   */
  def isMaybeBoxed(sym: Symbol) = {
    (sym == ObjectClass) ||
    (sym == JavaSerializableClass) ||
    (sym == ComparableClass) ||
    (sym isNonBottomSubClass BoxedNumberClass) ||
    (sym isNonBottomSubClass BoxedCharacterClass) ||
    (sym isNonBottomSubClass BoxedBooleanClass)
  }

  def newClassLoader(bin: ClassBinaryFile): loaders.SymbolLoader =
    new loaders.ClassfileLoader(bin)

  def doLoad(cls: ClassRep): Boolean = true

  def needCompile(classRep: ClassRep) = classRep match {
    case DualClassRep(src, bin) => src.lastModified >= bin.lastModified
    case _                      => false
  }
}
