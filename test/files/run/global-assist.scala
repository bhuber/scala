import scala.tools.util.GlobalAssist

object Test {
  val demo = """
package foo {
  class A {
    class Inner
    object Inner
    def method = 5
  }
  object B {
    class Inner
    object Inner
    def method = 10
  }

  abstract class Bippy extends Iterator[Int] {
  }
}
  """

  def main(args: Array[String]): Unit = {
    val args1 = if (args.isEmpty) List(demo) else args.toList
    val r = GlobalAssist.guess(args1: _*)
    import r.global._

    def dList(xs: List[Any])              = xs.map("" + _).sorted.distinct
    def dString(xs: List[Any])            = dList(xs).mkString("\n")
    def tString(s: Symbol)                = if (s.isDefinedInPackage) s.kind + " " + s.fullName else "%s in %s".format(s, s.owner.fullName)
    def symsString(xs: List[Symbol])      = dString(xs map (_.defString))
    def show(label: String, what: String) = println("\n// %s\n\n%s".format(label, what))

    show("Top Level", symsString(r.topLevel))
    show("All methods", symsString(r.methods))
    show("Referenced (public) symbols", dString(r.publicReferences map tString))
    show("Constant types", dString(r.constantTypes))
    show("Ident names", dList(r.idents.map(_.name)).mkString(" "))
  }
}
