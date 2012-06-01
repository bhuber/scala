/*  NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author
 */

package scala.tools.nsc
package transform

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import symtab.Flags._
import util.TreeSet

/** This phase converts classes with parameters into Java-like classes with
 *  fields, which are assigned to from constructors.
 */
abstract class Constructors extends Transform with ast.TreeDSL {
  import global._
  import definitions._

  /** the following two members override abstract members in Transform */
  val phaseName: String = "constructors"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ConstructorTransformer(unit)

  class ConstructorTransformer(unit: CompilationUnit) extends Transformer {

    def transformClassTemplate(impl: Template): Template = {
      val clazz = impl.symbol.owner  // the transformed class
      val stats = impl.body          // the transformed template body
      val localTyper = typer.atOwner(impl, clazz)

      case class ConstrInfo(
        constr: DefDef,               // The primary constructor
        constrParams: List[Symbol],   // ... and its parameters
        constrBody: Block             // ... and its body
      )
      // decompose primary constructor into the three entities above.
      val constrInfo: ConstrInfo = {
        stats find (_.symbol.isPrimaryConstructor) match {
          case Some(ddef @ DefDef(_, _, _, List(vparams), _, rhs @ Block(_, _))) =>
        ConstrInfo(ddef, vparams map (_.symbol), rhs)
          case x =>
            // AnyVal constructor is OK
            assert(clazz eq AnyValClass, "no constructor in template: impl = " + impl)
            return impl
        }
      }
      import constrInfo._

      // The parameter accessor fields which are members of the class
      val paramAccessors = clazz.constrParamAccessors

      // The constructor parameter corresponding to an accessor
      def parameter(acc: Symbol): Symbol =
        parameterNamed(nme.getterName(acc.originalName))

      // The constructor parameter with given name. This means the parameter
      // has given name, or starts with given name, and continues with a `$` afterwards.
      def parameterNamed(name: Name): Symbol = {
        def matchesName(param: Symbol) = param.name == name || param.name.startsWith(name + nme.NAME_JOIN_STRING)

        (constrParams filter matchesName) match {
          case Nil    => assert(false, name + " not in " + constrParams) ; null
          case p :: _ => p
        }
      }

      // A transformer for expressions that go into the constructor
      val intoConstructorTransformer = new Transformer {
        def isParamRef(sym: Symbol) =
          sym.isParamAccessor &&
          sym.owner == clazz &&
          !(clazz isSubClass DelayedInitClass) &&
          !(sym.isGetter && sym.accessed.isVariable) &&
          !sym.isSetter
        override def transform(tree: Tree): Tree = tree match {
          case Apply(Select(This(_), _), Nil) =>
            // references to parameter accessor methods of own class become references to parameters
            // outer accessors become references to $outer parameter
            if (isParamRef(tree.symbol))
              gen.mkAttributedIdent(parameter(tree.symbol.accessed)) setPos tree.pos
            else if (tree.symbol.outerSource == clazz && !clazz.isImplClass)
              gen.mkAttributedIdent(parameterNamed(nme.OUTER)) setPos tree.pos
            else
              super.transform(tree)
          case Select(This(_), _) if (isParamRef(tree.symbol)) =>
            // references to parameter accessor field of own class become references to parameters
            gen.mkAttributedIdent(parameter(tree.symbol)) setPos tree.pos
          case _ =>
            super.transform(tree)
        }
      }

      // Move tree into constructor, take care of changing owner from `oldowner` to constructor symbol
      def intoConstructor(oldowner: Symbol, tree: Tree) =
        intoConstructorTransformer transform tree.changeOwner(oldowner -> constr.symbol)

      // Should tree be moved in front of super constructor call?
      def canBeMoved(tree: Tree) = tree match {
        case ValDef(mods, _, _, _) => (mods hasFlag PRESUPER | PARAMACCESSOR)
        case _                     => false
      }

      // Create an assignment to class field `to` with rhs `from`
      def mkAssign(to: Symbol, from: Tree): Tree =
        localTyper.typedPos(to.pos) { Assign(Select(This(clazz), to), from) }

      // Create code to copy parameter to parameter accessor field.
      // If parameter is $outer, check that it is not null so that we NPE
      // here instead of at some unknown future $outer access.
      def copyParam(to: Symbol, from: Symbol): Tree = {
        import CODE._
        val result = mkAssign(to, Ident(from))

        if (from.name != nme.OUTER) result
        else localTyper.typedPos(to.pos) {
          IF (from OBJ_EQ NULL) THEN Throw(NullPointerExceptionClass.tpe) ELSE result
        }
      }

      // The list of definitions that go into class
      val defBuf = new ListBuffer[Tree]

      // The auxiliary constructors, separate from the defBuf since they should
      // follow the primary constructor
      val auxConstructorBuf = new ListBuffer[Tree]

      // The list of statements that go into constructor after and including the superclass constructor call
      val constrStatBuf = new ListBuffer[Tree]

      // The list of early initializer statements that go into constructor before the superclass constructor call
      val constrPrefixBuf = new ListBuffer[Tree]

      // The early initialized field definitions of the class (these are the class members)
      val presupers = treeInfo.preSuperFields(stats)

      // generate code to copy pre-initialized fields
      for (stat <- constrBody.stats) {
        constrStatBuf += stat
        stat match {
          case ValDef(mods, name, _, _) if (mods hasFlag PRESUPER) =>
            // stat is the constructor-local definition of the field value
            val fields = presupers filter (
              vdef => nme.localToGetter(vdef.name) == name)
            assert(fields.length == 1)
            val to = fields.head.symbol
            if (!to.tpe.isInstanceOf[ConstantType])
              constrStatBuf += mkAssign(to, Ident(stat.symbol))
          case _ =>
        }
      }

      // Triage all template definitions to go into defBuf/auxConstructorBuf, constrStatBuf, or constrPrefixBuf.
      for (stat <- stats) stat match {
        case DefDef(_,_,_,_,_,rhs) =>
          // methods with constant result type get literals as their body
          // all methods except the primary constructor go into template
          stat.symbol.tpe match {
            case MethodType(Nil, tp @ ConstantType(c)) =>
              defBuf += deriveDefDef(stat)(Literal(c) setPos _.pos setType tp)
            case _ =>
              if (stat.symbol.isPrimaryConstructor) ()
              else if (stat.symbol.isConstructor) auxConstructorBuf += stat
              else defBuf += stat
          }
        case ValDef(_, _, _, rhs) =>
          // val defs with constant right-hand sides are eliminated.
          // for all other val defs, an empty valdef goes into the template and
          // the initializer goes as an assignment into the constructor
          // if the val def is an early initialized or a parameter accessor, it goes
          // before the superclass constructor call, otherwise it goes after.
          // Lazy vals don't get the assignment in the constructor.
          if (!stat.symbol.tpe.isInstanceOf[ConstantType]) {
            if (rhs != EmptyTree && !stat.symbol.isLazy) {
              val rhs1 = intoConstructor(stat.symbol, rhs);
              (if (canBeMoved(stat)) constrPrefixBuf else constrStatBuf) += mkAssign(
                stat.symbol, rhs1)
            }
            defBuf += deriveValDef(stat)(_ => EmptyTree)
          }
        case ClassDef(_, _, _, _) =>
          // classes are treated recursively, and left in the template
          defBuf += new ConstructorTransformer(unit).transform(stat)
        case _ =>
          // all other statements go into the constructor
          constrStatBuf += intoConstructor(impl.symbol, stat)
      }

      // ----------- avoid making fields for symbols that are not accessed --------------

      // A sorted set of symbols that are known to be accessed outside the primary constructor.
      val accessedSyms = new TreeSet[Symbol]((x, y) => x isLess y)

      // a list of outer accessor symbols and their bodies
      var outerAccessors: List[(Symbol, Tree)] = Nil

      // Could symbol's definition be omitted, provided it is not accessed?
      // This is the case if the symbol is defined in the current class, and
      // ( the symbol is an object private parameter accessor field, or
      //   the symbol is an outer accessor of a final class which does not override another outer accessor. )
      def maybeOmittable(sym: Symbol) = sym.owner == clazz && (
        sym.isParamAccessor && sym.isPrivateLocal ||
        sym.isOuterAccessor && sym.owner.isEffectivelyFinal && !sym.isOverridingSymbol &&
        !(clazz isSubClass DelayedInitClass)
      )

      // Is symbol known to be accessed outside of the primary constructor,
      // or is it a symbol whose definition cannot be omitted anyway?
      def mustbeKept(sym: Symbol) = !maybeOmittable(sym) || (accessedSyms contains sym)

      // A traverser to set accessedSyms and outerAccessors
      val accessTraverser = new Traverser {
        override def traverse(tree: Tree) = {
          tree match {
            case DefDef(_, _, _, _, _, body)
            if (tree.symbol.isOuterAccessor && tree.symbol.owner == clazz && clazz.isEffectivelyFinal) =>
              debuglog("outerAccessors += " + tree.symbol.fullName)
              outerAccessors ::= ((tree.symbol, body))
            case Select(_, _) =>
              if (!mustbeKept(tree.symbol)) {
                debuglog("accessedSyms += " + tree.symbol.fullName)
                accessedSyms addEntry tree.symbol
              }
              super.traverse(tree)
            case _ =>
              super.traverse(tree)
          }
        }
      }

      // first traverse all definitions except outeraccesors
      // (outeraccessors are avoided in accessTraverser)
      for (stat <- defBuf.iterator ++ auxConstructorBuf.iterator)
        accessTraverser.traverse(stat)

      // then traverse all bodies of outeraccessors which are accessed themselves
      // note: this relies on the fact that an outer accessor never calls another
      // outer accessor in the same class.
      for ((accSym, accBody) <- outerAccessors)
        if (mustbeKept(accSym)) accessTraverser.traverse(accBody)

      // Initialize all parameters fields that must be kept.
      val paramInits = paramAccessors filter mustbeKept map { acc =>
        // Check for conflicting symbol amongst parents: see bug #1960.
        // It would be better to mangle the constructor parameter name since
        // it can only be used internally, but I think we need more robust name
        // mangling before we introduce more of it.
        val conflict = clazz.info.nonPrivateMember(acc.name) filter (s => s.isGetter && !s.isOuterField && s.enclClass.isTrait)
        if (conflict ne NoSymbol)
          unit.error(acc.pos, "parameter '%s' requires field but conflicts with %s".format(acc.name, conflict.fullLocationString))

        copyParam(acc, parameter(acc))
      }

      /** Create a getter or a setter and enter into `clazz` scope
       */
      def addAccessor(sym: Symbol, name: TermName, flags: Long) = {
        val m = clazz.newMethod(name, sym.pos, flags & ~(LOCAL | PRIVATE)) setPrivateWithin clazz
        clazz.info.decls enter m
      }

      def addGetter(sym: Symbol): Symbol = {
        val getr = addAccessor(
          sym, nme.getterName(sym.name), getterFlags(sym.flags))
        getr setInfo MethodType(Nil, sym.tpe)
        defBuf += localTyper.typedPos(sym.pos)(DefDef(getr, Select(This(clazz), sym)))
        getr
      }

      def addSetter(sym: Symbol): Symbol = {
        sym setFlag MUTABLE
        val setr = addAccessor(
          sym, nme.getterToSetter(nme.getterName(sym.name)), setterFlags(sym.flags))
        setr setInfo MethodType(setr.newSyntheticValueParams(List(sym.tpe)), UnitClass.tpe)
        defBuf += localTyper.typed {
          //util.trace("adding setter def for "+setr) {
          atPos(sym.pos) {
            DefDef(setr, paramss =>
              Assign(Select(This(clazz), sym), Ident(paramss.head.head)))
          }//}
        }
        setr
      }

      def ensureAccessor(sym: Symbol)(acc: => Symbol) =
        if (sym.owner == clazz && !sym.isMethod && sym.isPrivate) { // there's an access to a naked field of the enclosing class
          var getr = acc
          getr makeNotPrivate clazz
          getr
        } else {
          if (sym.owner == clazz) sym makeNotPrivate clazz
          NoSymbol
        }

      def ensureGetter(sym: Symbol): Symbol = ensureAccessor(sym) {
        val getr = sym.getter(clazz)
        if (getr != NoSymbol) getr else addGetter(sym)
      }

      def ensureSetter(sym: Symbol): Symbol = ensureAccessor(sym) {
        var setr = sym.setter(clazz, hasExpandedName = false)
        if (setr == NoSymbol) setr = sym.setter(clazz, hasExpandedName = true)
        if (setr == NoSymbol) setr = addSetter(sym)
        setr
      }

      def delayedInitClosure(stats: List[Tree]) =
        localTyper.typed {
          atPos(impl.pos) {
            val closureClass   = clazz.newClass(nme.delayedInitArg.toTypeName, impl.pos, SYNTHETIC | FINAL)
            val closureParents = List(AbstractFunctionClass(0).tpe)

            closureClass setInfoAndEnter new ClassInfoType(closureParents, newScope, closureClass)

            val outerField = (
              closureClass
                newValue(nme.OUTER, impl.pos, PrivateLocal | PARAMACCESSOR)
                setInfoAndEnter clazz.tpe
            )
            val applyMethod = (
              closureClass
                newMethod(nme.apply, impl.pos, FINAL)
                setInfoAndEnter MethodType(Nil, ObjectClass.tpe)
            )
            val outerFieldDef     = ValDef(outerField)
            val closureClassTyper = localTyper.atOwner(closureClass)
            val applyMethodTyper  = closureClassTyper.atOwner(applyMethod)

            val constrStatTransformer = new Transformer {
              override def transform(tree: Tree): Tree = tree match {
                case This(_) if tree.symbol == clazz =>
                  applyMethodTyper.typed {
                    atPos(tree.pos) {
                      Select(This(closureClass), outerField)
                    }
                  }
                case _ =>
                  super.transform {
                    tree match {
                      case Select(qual, _) =>
                        val getter = ensureGetter(tree.symbol)
                        if (getter != NoSymbol)
                          applyMethodTyper.typed {
                            atPos(tree.pos) {
                              Apply(Select(qual, getter), Nil)
                            }
                          }
                        else tree
                      case Assign(lhs @ Select(qual, _), rhs) =>
                        val setter = ensureSetter(lhs.symbol)
                        if (setter != NoSymbol)
                          applyMethodTyper.typed {
                            atPos(tree.pos) {
                              Apply(Select(qual, setter), List(rhs))
                            }
                          }
                        else tree
                      case _ =>
                        tree.changeOwner(impl.symbol -> applyMethod)
                    }
                  }
              }
            }

            def applyMethodStats = constrStatTransformer.transformTrees(stats)

            val applyMethodDef = DefDef(
              sym = applyMethod,
              vparamss = List(Nil),
              rhs = Block(applyMethodStats, gen.mkAttributedRef(BoxedUnit_UNIT)))

            ClassDef(
              sym = closureClass,
              constrMods = Modifiers(0),
              vparamss = List(List(outerFieldDef)),
              argss = List(Nil),
              body = List(applyMethodDef),
              superPos = impl.pos)
          }
        }

      def delayedInitCall(closure: Tree) = localTyper.typedPos(impl.pos) {
        gen.mkMethodCall(This(clazz), delayedInitMethod, Nil, List(New(closure.symbol.tpe, This(clazz))))
      }

      /** Return a pair consisting of (all statements up to and including superclass and trait constr calls, rest) */
      def splitAtSuper(stats: List[Tree]) = {
        def isConstr(tree: Tree) = (tree.symbol ne null) && tree.symbol.isConstructor
        val (pre, rest0) = stats span (!isConstr(_))
        val (supercalls, rest) = rest0 span (isConstr(_))
        (pre ::: supercalls, rest)
      }

      var (uptoSuperStats, remainingConstrStats) = splitAtSuper(constrStatBuf.toList)

      /** XXX This is not corect: remainingConstrStats.nonEmpty excludes too much,
       *  but excluding it includes too much.  The constructor sequence being mimicked
       *  needs to be reproduced with total fidelity.
       *
       *  See test case files/run/bug4680.scala, the output of which is wrong in many
       *  particulars.
       */
      val needsDelayedInit =
        (clazz isSubClass DelayedInitClass) /*&& !(defBuf exists isInitDef)*/ && remainingConstrStats.nonEmpty

      if (needsDelayedInit) {
        val dicl = new ConstructorTransformer(unit) transform delayedInitClosure(remainingConstrStats)
        defBuf += dicl
        remainingConstrStats = List(delayedInitCall(dicl))
      }

      // Assemble final constructor
      defBuf += deriveDefDef(constr)(_ =>
        treeCopy.Block(
          constrBody,
          paramInits ::: constrPrefixBuf.toList ::: uptoSuperStats ::: remainingConstrStats,
          constrBody.expr))

      // Followed by any auxiliary constructors
      defBuf ++= auxConstructorBuf

      // Unlink all fields that can be dropped from class scope
      for (sym <- clazz.info.decls ; if !mustbeKept(sym))
        clazz.info.decls unlink sym

      // Eliminate all field definitions that can be dropped from template
      deriveTemplate(impl)(_ => defBuf.toList filter (stat => mustbeKept(stat.symbol)))
    } // transformClassTemplate

    override def transform(tree: Tree): Tree =
      tree match {
        case ClassDef(_,_,_,_) if !tree.symbol.isInterface && !isPrimitiveValueClass(tree.symbol) =>
          deriveClassDef(tree)(transformClassTemplate)
        case _ =>
          super.transform(tree)
      }
  } // ConstructorTransformer
}
