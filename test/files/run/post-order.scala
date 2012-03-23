abstract class A {
  def foo: String
  println("A#init: " + foo + "   (" + getClass + ")")
}
object B1 extends A {
  val foo = "bar"
  println("B1#init: " + foo)
}
object B2 extends A {
  lazy val foo = "bar"
  println("B2#init: " + foo)
}
object B3 extends A {
  def foo = "bar"
  println("B3#init: " + foo)
}
object B4 extends { val foo = "bar" } with A {
  println("B4#init: " + foo)
}

class C1 extends A {
  val foo = "bar"
  println("C1#init: " + foo)
}
class C2 extends A {
  lazy val foo = "bar"
  println("C2#init: " + foo)
}
class C3 extends A {
  def foo = "bar"
  println("C3#init: " + foo)
}
class C4 extends { val foo = "bar" } with A {
  println("C3#init: " + foo)
}

class D(val foo: String) extends A {
  println("D#init: " + foo)
}


object Test {  
  def main(args: Array[String]): Unit = {
    val a1 = new A { val foo = "bar" }
    val a2 = new { val foo = "bar" } with A
    
    val b1 = B1
    val b2 = B2
    val b3 = B3
    val b4 = B4
    
    val c1 = new C1
    val c2 = new C2
    val c3 = new C3
    val c4 = new C4

    val d = new D("bar")
  }
}
