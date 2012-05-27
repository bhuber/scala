trait ContraBippy[-T]

class A
object A {
  implicit object AOrdering extends Ordering[A] { def compare(x: A, y: A) = 0 ; override def toString = "AOrdering" }
  implicit object ABippy extends ContraBippy[A] { override def toString = "ABippy" }
}

class B extends A
object B {
  implicit object BOrdering extends Ordering[B] { def compare(x: B, y: B) = 0 ; override def toString = "BOrdering" }
  implicit object BBippy extends ContraBippy[B] { override def toString = "BBippy" }
}

object Test {
  def f[T: Ordering](x: T) = println(implicitly[Ordering[T]])
  def g[T: ContraBippy](x: T) = println(implicitly[ContraBippy[T]])

  def main(args: Array[String]): Unit = {
    println(implicitly[Ordering[A]])
    println(implicitly[Ordering[B]])
    println(implicitly[ContraBippy[A]])
    println(implicitly[ContraBippy[B]])
    f(new A)
    f(new B)
    g(new A)
    g(new B)
  }
}
