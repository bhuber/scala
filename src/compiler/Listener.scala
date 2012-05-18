package improving

import scala.tools.nsc._
import scala.tools.nsc.backend.jvm.JvmListener
import scala.collection.{ mutable, immutable }
import scala.tools.util.StringOps.trimTrailingSpace

class Listener[T <: Global](val global: T) extends JvmListener {
  import global._
  import definitions._
  import ch.epfl.lamp.fjbg._

  object util extends genJVM.BytecodeUtil { }
  import util._

  val basedir = io.Directory(settings.outdir.value) / "sources" createDirectory();
  val javaReserved = """
    abstract assert boolean break byte case catch char class const
    continue default do double else enum extends final finally float for
    goto if implements import instanceof int interface long native new
    package private protected public return short static strictfp super
    switch synchronized this throw throws transient try void volatile while
  """.split("\\s+").toSet

  private def mkPackage(pkg: Symbol): PackageInfo = {
    assert(pkg ne NoSymbol)
    if (pkg == RootClass) RootPackageInfo
    else PackageInfo(packages(pkg.enclosingPackageClass), pkg)
  }

  val packageBuffer = mutable.Map[Symbol, PackageInfo]()

  def packages(sym: Symbol): PackageInfo = {
    val pkg = if (sym.isPackageClass) sym else sym.enclosingPackageClass
    packageBuffer.getOrElseUpdate(pkg, mkPackage(pkg))
  }
  def lookupClass(clazz: Symbol) = {
    lookup(clazz) match {
      case info: ClassOrInterfaceInfo => info
      case info                       => sys.error("lookup(" + clazz + ") == " + info)
    }
  }
  def lookup(sym: Symbol): SymbolInfo = sym match {
    case NoSymbol => NoSymbolInfo
    case _        => packages(sym) find (_.symbol == sym)
  }

  def dump() {
    packageBuffer.values.toList.sortBy(_.fullName.toString) foreach { pkg =>
      pkg.classes filter (_.symbol.isDefinedInPackage) foreach { clazz =>
        clazz.file writeAll clazz.classDefinition
        println("Wrote " + clazz.file.toAbsolute)
      }
    }
  }

  def noReserved(name: String): String = if (javaReserved(name)) "$" + name else name
  def noReserved(name: Any): String    = noReserved("" + name)

  case class CallInfo(receiver: Symbol, method: Symbol, signature: String) {
    private def defString = method.defStringSeenAs(receiver.tpe memberType method)
    def unerased = beforeErasure(defString)
    def erased   = defString
  }

  object NoSymbolInfo extends SymbolInfo {
    def owner = this
    def symbol = NoSymbol
    def members = Nil
    def symbolTypes = Nil
  }
  object RootPackageInfo extends PackageInfo(null, RootClass) {

  }

  trait SymbolInfo {
    def owner: SymbolInfo
    def symbol: Symbol
    def fullName = symbol.fullName
    def members: List[SymbolInfo]
    def symbolTypes: List[Type]

    def visibleTypes: List[Type] = symbolTypes ++ members.flatMap(_.symbolTypes) distinct

    def find(p: SymbolInfo => Boolean): SymbolInfo = this :: members find p getOrElse NoSymbolInfo
    def ownerChain: List[SymbolInfo] = this :: (owner match {
      case null => Nil
      case _    => owner.ownerChain
    })
    def pkg            = if (symbol.isPackageClass) symbol else symbol.enclosingPackageClass
    def modifiers      = List("public")
    def enclClass      = ownerChain collectFirst { case x: ClassOrInterfaceInfo => x } orNull
    def enclPackage    = ownerChain collectFirst { case x: PackageInfo => x } orNull
    def isPublic       = symbol.isPublic && !symbol.isAnonymousClass
    def name           = noReserved(symbol.javaSimpleName)

    def modifierString = modifiers.map(_ + " ").mkString
    def isUniquelyNamed(sym: Symbol) = enclClass.uniqueNamesInClass(sym.javaSimpleName)
    def nameOfType(tp: Type): String = ({
      val sym = tp.typeSymbol

      if (isPrimitiveValueClass(sym) || (sym == ArrayClass))
        "" + javaType(tp)
      else
        noReserved(
          if (isUniquelyNamed(sym)) sym.javaSimpleName
          else sym.javaClassName
        )
    }).replace('/', '.')
    override def toString = "" + symbol
  }

  abstract class ClassOrInterfaceInfo(kind: String) extends SymbolInfo {
    val file = io.File(basedir / ("" + symbol.javaBinaryName + ".java"))
    if (!file.parent.isDirectory)
      file.parent.createDirectory()

    def classDefinition = (
      """|package %s;
         |
         |%s%s {
         |%s%s
         |}
         |""".stripMargin.format(
           pkg.fullName,
           if (imports.isEmpty) "" else imports.mkString("", "\n", "\n\n"),
           trimTrailingSpace(definition),
           (fields ++ methods).map("  " + _ + "\n").mkString,
           if (bridges.isEmpty) "" else bridges.mkString("\n  // Bridge methods\n  // ", "\n  // ", "\n")
         )
    )

    private var memberBuffer: List[SymbolInfo] = Nil
    private var callBuffer: List[CallInfo] = Nil
    def add(member: Symbol, signature: String): this.type = {
      memberBuffer ::= (
        if (member.isClass) ClassInfo(this, member, signature)
        else if (member.isMethod) MethodInfo(this, member, signature)
        else FieldInfo(this, member, signature)
      )
      this
    }
    def addCall(info: CallInfo): this.type = {
      callBuffer ::= info
      this
    }
    def members = memberBuffer.reverse
    def fields  = members collect { case x: FieldInfo => x }
    def bridges = members collect { case x: MethodInfo if  x.isBridge => x }
    def methods = members collect { case x: MethodInfo if !x.isBridge => x }

    def definition       = modifiers :+ kind :+ name :+ parentString mkString " "
    def symbolTypes      = symbol.info :: parents
    def parents          = symbol.info.parents filterNot (_.typeSymbol == ObjectClass)
    def parentClasses    = parents filterNot (_.typeSymbol.isInterface)
    def parentInterfaces = parents filter (_.typeSymbol.isInterface)
    def parentNames      = parents map nameOfType
    def unerasedParents  = beforeErasure(parents map (tp => tp.typeSymbol.defStringSeenAs(symbol.tpe)))

    lazy val uniqueNamesInClass: Set[Name] = (
      visibleTypes
        groupBy (_.typeSymbol.javaSimpleName)
        collect { case (k, vs) if vs.tail.isEmpty => k }
          toSet
    )

    private val unqualifiedPackages = Set(enclPackage, JavaLangPackageClass)
    private def noImport(tp: Type) = {
      val sym = tp.typeSymbol
      (    isPrimitiveValueClass(sym)
        || unqualifiedPackages(sym.enclosingPackageClass) && isUniquelyNamed(sym)
      )
    }
    def imports = (
      visibleTypes filterNot noImport
              map (t => "import %s;".format(t.typeSymbol.fullName))
           sorted
    )

    def indentDecl(s: String) = "%25s".format(s + " ")
    def extendsString: String
    def implementsString: String
    def parentString = extendsString + implementsString
    override def toString = definition
  }

  abstract class MemberInfo extends SymbolInfo {
    def signature: String
    def isConstructor   = symbol.isClassConstructor
    def isBridge        = symbol.isBridge
    def finalResultType = symbol.info.resultType
    def symbolTypes     = finalResultType :: params.map(_.tpe)
    def params          = symbol.info.paramss.flatten
    def paramsString    = ""

    def definition = "%s%-40s %s%s;".format(
      modifierString, nameOfType(finalResultType), name, paramsString
    )
    // def warning = "/*** Actual member name %s is java reserved word. ***/".format(method.name)
    override def toString = "%-80s%s".format(definition, " // " + signature)
  }

  case class ClassInfo(owner: SymbolInfo, symbol: Symbol, signature: String) extends ClassOrInterfaceInfo("class") {
    override def extendsString = parentClasses match {
      case Nil      => ""
      case x :: Nil => "extends " + nameOfType(x)
      case xs       => sys.error(xs mkString ", ")
    }
    override def implementsString = parentInterfaces match {
      case Nil    => ""
      case ps     => "\n" + ps.map(nameOfType).mkString(indentDecl("implements"), ",", "")
    }
    override def modifiers = super.modifiers :+ "abstract"
  }
  case class InterfaceInfo(owner: SymbolInfo, symbol: Symbol, signature: String) extends ClassOrInterfaceInfo("interface") {
    override def extendsString = parentInterfaces match {
      case Nil  => ""
      case ps   => "\n" + ps.map(nameOfType).mkString(indentDecl("extends"), ",", "")
    }
    override def implementsString = ""
  }
  case class MethodInfo(owner: ClassOrInterfaceInfo, symbol: Symbol, signature: String) extends MemberInfo {
    def members = Nil
    override def name                  = if (isConstructor) owner.name else super.name
    override def paramsString          = params map paramString mkString ("(", ", ", ")")
    private def paramString(p: Symbol) = nameOfType(p.tpe) + " " + noReserved(p.name)
  }
  case class FieldInfo(owner: ClassOrInterfaceInfo, symbol: Symbol, signature: String) extends MemberInfo {
    def members = Nil
  }

  case class PackageInfo(owner: PackageInfo, symbol: Symbol) extends SymbolInfo {
    private var classBuffer: List[ClassOrInterfaceInfo] = Nil
    def symbolTypes = Nil
    def members = classBuffer.reverse
    def classes = members
    def add(clazz: Symbol, signature: String): this.type = {
      classBuffer ::= (
        if (clazz.isInterface) InterfaceInfo(this, clazz, signature)
        else ClassInfo(this, clazz, signature)
      )
      this
    }
  }
  def printResult[T](msg: String)(body: => T): T = {
    print(msg + " ")
    val result = body
    println(result)
    result
  }
  def registerClass(clazz: Symbol, signature: String) = /*printResult("registerClass" + ((clazz, signature))) */{
    clazz.info
    packages(clazz).add(clazz, signature)
  }
  def registerMember(clazz: Symbol, member: Symbol, signature: String) = /*printResult("registerMember" + ((clazz, member, signature))) */{
    clazz.info
    member.info
    lookupClass(clazz).add(member, signature)
  }
  def registerCall(clazz: Symbol, receiver: Symbol, method: Symbol, signature: String) = {
    clazz.info
    receiver.info
    method.info
    /*printResult("registerCall" + ((clazz, receiver, method, signature))) */
    lookupClass(clazz) addCall CallInfo(receiver, method, signature)
  }

  sys addShutdownHook dump()
}
