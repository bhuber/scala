package improving

import scala.tools.nsc._
import scala.tools.nsc.backend.jvm.JvmListener
import scala.collection.{ mutable, immutable }

class Listener[T <: Global](val global: T) extends JvmListener {
  import global._
  import definitions._
  import ch.epfl.lamp.fjbg._

  val javaReserved = """
    abstract assert boolean break byte case catch char class const
    continue default do double else enum extends final finally float for
    goto if implements import instanceof int interface long native new
    package private protected public return short static strictfp super
    switch synchronized this throw throws transient try void volatile while
  """.split("\\s+").toSet

  case class MemberInfo(owner: Symbol, member: Symbol, signature: String) { }

  val packages = mutable.Map[Symbol, List[MemberInfo]]() withDefaultValue Nil
  val members  = mutable.Map[Symbol, List[MemberInfo]]() withDefaultValue Nil
  val calls    = mutable.Map[Symbol, List[MemberInfo]]() withDefaultValue Nil
  val revcalls = mutable.Map[Symbol, List[MemberInfo]]() withDefaultValue Nil

  // val out = calls(m) filterNot { case (recv, m) => (recv == clazz) || (m.owner == jlObject) }
  // val in  = revcalls(m) filterNot { case (caller, m) => (caller == clazz) }
  //
  // if (out.nonEmpty) {
  //   println("      Outcalls:")
  //   for ((clazz, method) <- out) {
  //     val signature = method.defStringSeenAs(clazz.tpe memberType method)
  //     println("      " + signature)
  //     //
  //     // val pstr = method.paramTypes.mkString("(", ";", ")")
  //     // println("      " + (
  //     //   if (method.isClassConstructor) clazz.fullName + ".<init>" + pstr
  //     //   else clazz.fullName + "." + method.name + pstr)
  //     // )
  //   }
  // }
  // if (in.nonEmpty) {
  //   println("       Incalls:")
  //   for ((clazz, method) <- in) {
  //     val signature = method.defStringSeenAs(clazz.tpe memberType method)
  //     println("      " + signature)
  //   }
  //     // println("      %s.%s".format(clazz.fullName, method.defString))
  // }
  // val parents = beforeErasure(
  //   clazz.info.parents map (tp => tp.typeSymbol.defStringSeenAs(clazz.tpe))
  // )
  // println("  " + clazz.defString + " {") // }
  // println("  %s %s %s extends %s".format(
  //   clazz.accessString,
  //   kind,
  //   clazz.decodedName,
  //   parents mkString " with "
  // ))

  def dump() {
    val jlObject = definitions.requiredClass[Object]
    object util extends genJVM.BytecodeUtil { }
    import util._

    packages.toList foreach { case (pkg, classes) =>
      def typeName(tp: Type): String = {
        val sym = tp.typeSymbol
        if (isPrimitiveValueClass(sym)) "" + javaType(tp)
        else if (sym.enclosingPackage == pkg) "" + sym.javaSimpleName
        else sym.fullName.stripPrefix("java.lang.")
      }
      def termName(sym: Symbol) = {
        if (sym.isClassConstructor) "" else {
          val name = sym.name.toString
          if (javaReserved(name)) "$" + name else name
        }
      }

      classes filter (_.member.isPublic) foreach { case MemberInfo(pkg, clazz, signature) =>
        val isInterface = clazz.isInterface || clazz.info.parents.isEmpty
        val kind        = if (isInterface) "interface" else "abstract class"
        val parents     = clazz.info.parents.filterNot(_.typeSymbol == jlObject) map typeName

        val parentString = parents match {
          case Nil      => ""
          case p :: Nil => "\n        extends " + p
          case p :: ps  => "\n     %s".format(
            if (isInterface) "   extends " + (p :: ps).mkString(",")
            else             "   extends " + p + "\n     implements " + ps.mkString(", ")
          )
        }

        println("package " + pkg.fullName + ";\n")
        println("public %s %s%s".format(
          kind,
          clazz.javaSimpleName,
          parentString) + " {" // }
        )

        val toPrint = members(clazz) filter (_.member.isPublic) sortBy (x => !x.member.isClassConstructor)

        toPrint foreach { case MemberInfo(_, method, signature) =>
          val resType     = method.info.finalResultType
          val params      = method.paramss.flatten
          def paramString = params map (p => typeName(p.tpe) + " " + termName(p)) mkString ", "

          def show(msg: String, sig: String) {
            if (javaReserved(method.name.toString)) {
              println("  /*** Actual member name %s is java reserved word. ***/".format(method.name))
            }
            if (method.isClassConstructor)
              println("  public %-70s // %s".format(msg, sig))
            else
              println("  public %-70s // %s".format(msg, sig))
          }

          javaType(method) match {
            case jtype: JMethodType =>
              if (method.isClassConstructor) {
                if (params.isEmpty) ()
                else show("%-15s %s(%s);".format(typeName(resType), clazz.javaSimpleName, paramString), signature)
              }
              else show("%-15s %s(%s);".format(typeName(resType), termName(method), paramString), signature)
            case jtype =>
              show("%-15s %s;".format(typeName(resType), termName(method)), signature)
          }
        }
        println("}\n")
      }
    }
  }

  sys addShutdownHook dump()

  def registerClass(clazz: Symbol, signature: String) {
    packages(clazz.enclosingPackage) ::= MemberInfo(clazz.enclosingPackage, clazz, signature)
  }
  def registerMember(clazz: Symbol, member: Symbol, signature: String) {
    members(clazz) ::= MemberInfo(clazz, member, signature)
  }
  def registerCall(from: ClassAndMethod, to: ClassAndMethod, signature: String) {
    calls(from._2) ::= MemberInfo(to._1, to._2, signature)
    revcalls(to._2) ::= MemberInfo(from._1, from._2, "")
  }
}
