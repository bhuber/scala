trait MyOrdering[-T] {
  def compare(x: T, y: T): Int
  def lt(x: T, y: T) = compare(x, y) < 0
}
object MyOrdering {
  implicit def Iterable[T](implicit ord: Ordering[T]): MyOrdering[Iterable[T]] =
    new MyOrdering[Iterable[T]] {
      def compare(x: Iterable[T], y: Iterable[T]): Int = {
        val xe = x.iterator
        val ye = y.iterator

        while (xe.hasNext && ye.hasNext) {
          val res = ord.compare(xe.next, ye.next)
          if (res != 0) return res
        }

        xe.hasNext compare ye.hasNext
      }
    }
}

object Test {
  class CmpCmp[T: MyOrdering](x: T) { def < (y: T) = implicitly[MyOrdering[T]].lt(x, y) }
  implicit def compareComparables[T: MyOrdering](x: T) = new CmpCmp[T](x)

  def f1 = {
    println("Iterable: " + (Iterable("a") < Iterable("b")))
    println("Seq: " + (Seq("a") < Seq("b")))
    println("List: " + (List("a") < List("b")))
  }

  def f2 = {
    implicit object myOrd extends MyOrdering[Seq[String]] {
      def compare(x: Seq[String], y: Seq[String]) = {
        val res = implicitly[MyOrdering[Iterable[String]]].compare(x, y)
        if (res < 0) 1
        else if (res > 0) -1
        else 0
      }
    }

    println("Iterable: " + (Iterable("a") < Iterable("b")))
    println("Seq: " + (Seq("a") < Seq("b")))
    println("List: " + (List("a") < List("b")))
  }

  def main(args: Array[String]): Unit = {
    f1
    f2
  }
}
