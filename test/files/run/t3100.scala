// trait TraitOfEnrichment {
//   def x: Int
// }
final class ClassOfEnrichment(@inline val x: String) {
  @inline def something = 15 + x.length
}

object Test {
  @inline implicit def enrichMe(str: String): ClassOfEnrichment =
    new ClassOfEnrichment(str)

  def f0 = "string".something
  def f1 = {
    val str = "string"
    enrichMe(str).something
  }
  def f2 = {
    val str = "string"
    val clazz = new ClassOfEnrichment(str)
    clazz.something
  }

  def main(args: Array[String]): Unit = {
    println(f0)
    println(f1)
    println(f2)
  }
}
