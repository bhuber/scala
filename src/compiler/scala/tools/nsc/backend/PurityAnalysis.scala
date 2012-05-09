/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package backend

import scala.collection.mutable
import scala.tools.nsc.symtab._
import scala.tools.nsc.util.NoSourceFile

trait PurityAnalysis {
  val global: Global

  import global._
  import icodes._
  import icodes.opcodes._
  import definitions._

  /** Look up implementation of method 'sym in 'clazz'.
   */
  def lookupImplFor(sym: Symbol, clazz: Symbol): Symbol = {
    // TODO: verify that clazz.superClass is equivalent here to clazz.tpe.parents(0).typeSymbol (.tpe vs .info)
    def needsLookup = (
         (clazz != NoSymbol)
      && (clazz != sym.owner)
      && !sym.isEffectivelyFinal
      && clazz.isEffectivelyFinal
    )
    def lookup(clazz: Symbol): Symbol = {
      // println("\t\tlooking up " + meth + " in " + clazz.fullName + " meth.owner = " + meth.owner)
      if (sym.owner == clazz || clazz == NullClass || clazz == NothingClass) sym
      else sym.overridingSymbol(clazz) orElse {
        if (sym.owner.isTrait) sym
        else lookup(clazz.superClass)
      }
    }
    if (needsLookup) {
      val concreteMethod = lookup(clazz)
      debuglog("\tlooked up method: " + concreteMethod.fullName)

      concreteMethod
    }
    else sym
  }

  def isPureInstruction(instr: Instruction): Boolean = logResult("isPureInstruction(" + instr + ")") {
    instr match {
      case THIS(clasz)                          => true
      case CONSTANT(const)                      => true
      case LOAD_LOCAL(local)                    => true
      case LOAD_FIELD(field, isStatic)          => true
      case LOAD_MODULE(module)                  => true
      case CALL_PRIMITIVE(primitive)            => true
      case IS_INSTANCE(tpe)                     => true
      case CHECK_CAST(tpe)                      => true
      case JUMP(whereto)                        => true
      case CJUMP(success, failure, cond, kind)  => true
      case CZJUMP(success, failure, cond, kind) => true
      case RETURN(kind)                         => true
      case BOX(boxType)                         => true
      case UNBOX(tpe)                           => true
      case SCOPE_ENTER(lv)                      => true
      case SCOPE_EXIT(lv)                       => true
      case DUP(REFERENCE(cls))                  => true
      case STORE_LOCAL(local)                   => true
      case STORE_THIS(kind)                     => true
      case SWITCH(tags, labels)                 => true
      case DROP(kind)                           => true
      case LOAD_ARRAY_ITEM(kind)                => true
      case LOAD_EXCEPTION(clasz)                => true
      case STORE_FIELD(field, isStatic)         => field.isEffectivelyFinal && !field.isMutable
      case CALL_METHOD(method, style)           => isPureMethodCall(method)
      case NEW(REFERENCE(cls))                  => isPureAllocation(cls)

      case _ => instr.isInstanceOf[DupX]
      // case STORE_ARRAY_ITEM(kind)               =>
      // case CREATE_ARRAY(elem, dims)             =>
      // case THROW(clasz)                         =>
      // case MONITOR_ENTER()                      =>
      // case MONITOR_EXIT()                       =>
    }
  }

  def loadIMethod(sym: Symbol): Option[IMethod] = logResult("loadIMethod" + ((sym, sym.companionSymbol))) {
    val iclass   = logResult("load " + sym)(icodes.loadIfPossible(sym.enclClass))
    val imethods = iclass.toList flatMap (_.methods)

    imethods filter (_.symbol.name == sym.name) match {
      case imethod :: Nil => Some(imethod)
      case xs             =>
        imethods foreach (m => println("  " + m.symbol.defString))
        logResult("loadIMethod found " + xs)(None)
    }
  }

  def isPureAllocation(clazz: Symbol): Boolean = logResult("isPureAllocation(" + clazz.defString + ")")(
       ((clazz eq ObjectClass) || (clazz eq AnyClass))
    || (isPrimitiveValueClass(clazz))
    || (clazz.isInterface)
    || (
            (clazz.isEffectivelyFinal)
         && (clazz.info.decls filter (_.isClassConstructor) forall isPureMethodDefinition)
         && (clazz.parentSymbols forall isPureAllocation)
       )
  )
  def isPureSelection(prefix: Symbol) = logResult("isPureSelection(" + prefix.defString + ")")(
       prefix.isStatic
    || prefix.isClass
    || (prefix.isModule && isPureAllocation(prefix.moduleClass))
  )

  def isPureIMethod(imethod: IMethod): Boolean = logResult("isPureIMethod(" + imethod.symbol.defString + " in " + imethod.enclClass + ")")(
    imethod.hasCode && (imethod.instructions forall isPureInstruction)
  )
  def isPureMethodDefinition(sym: Symbol): Boolean = (
       sym.isDeferred
    || (loadIMethod(sym) exists isPureIMethod)
  )

  def isPureMethodCall(sym: Symbol): Boolean = logResult("isPureMethodCall(" + sym.defString + " in " + sym.owner + ")")(
    sym.isMethod && !sym.isLazy && (
       ((sym.owner eq ObjectClass) || (sym.owner eq AnyClass) || isPrimitiveValueClass(sym.owner))
    || (sym.isGetter && sym.isEffectivelyFinal)
    || (sym.isPrimaryConstructor && (sym.enclosingPackage == RuntimePackage || inliner.isClosureClass(sym.owner)))
    || (isPureSelection(sym.owner) && isPureMethodDefinition(sym))
  ))

  def isSideEffecting(sym: Symbol): Boolean = !isPureMethodCall(sym)
}
