object Test extends Foo {
  val value = "abc"
  println("Test#init, value = " + value)
  
  def main(args: Array[String]): Unit = {
    println("Test#main, value = " + value)
    val c = new C
  }
}

abstract class Foo extends PostConstructor {
  def value: String
  println("Foo#init, value = " + value)

  protected def postConstructor() {
    // value.length would NPE in constructor
    println("Foo#postConstructor, value = " + value + " (with length " + value.length + ")")
  }
}

trait A extends PostConstructor {
  println("A ConstructionCode")
  protected def postConstructor = println("A PostConstructionCode")
}
trait B extends A {
  println("B ConstructionCode")
  override protected def postConstructor = { super.postConstructor ; println("B PostConstructionCode") }
} 
  
class C extends B { 
  println("C ConstructionCode")
  override protected def postConstructor = { super.postConstructor ; println("C PostConstructionCode") }
}
