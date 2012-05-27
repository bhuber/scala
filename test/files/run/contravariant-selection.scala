trait Ord[-T]

class OverloadSelection {
  // The return types vary to overcome "same type after erasure".
  def f(x: Ord[Iterable[Int]]): Int    = 1
  def f(x: Ord[     Seq[Int]]): Byte   = 2
  def f(x: Ord[    List[Int]]): Short  = 3
  
  println("Static overloading selection: " + List(
    f(new Ord[Iterable[Int]] { }),
    f(new Ord[     Seq[Int]] { }),
    f(new Ord[    List[Int]] { })
  ).mkString("  "))
}

class ImplicitSelection {
  implicit val Ord1: Ord[Iterable[Int]] = new Ord[Iterable[Int]] { override def toString = "1" }
  implicit val Ord2: Ord[     Seq[Int]] = new Ord[     Seq[Int]] { override def toString = "2" }
  implicit val Ord3: Ord[    List[Int]] = new Ord[    List[Int]] { override def toString = "3" }
  
  println("    Implicit value selection: " + List(
    implicitly[Ord[Iterable[Int]]],
    implicitly[Ord[     Seq[Int]]],
    implicitly[Ord[    List[Int]]]
  ).mkString("  "))
}

object Test {
  def main(args: Array[String]): Unit = {
    println("""
      |If there are several eligible arguments which match the
      |implicit parameter’s type, a most specific one will be
      |chosen using the rules of static overloading resolution.
      |  -- SLS 7.2, "Implicit Parameters"
      |""".stripMargin
    )
    new OverloadSelection
    new ImplicitSelection
  }
}

  
