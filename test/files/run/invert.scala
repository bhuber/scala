import collection.GenTraversableOnce

object Test {
  def showAny(x: Any) = x match {
    case x: Array[_] => x.mkString("Array(", ", ", ")")
    case x           => "" + x
  }
  def show(x: Tuple3[_, _, _]) = {
    showAny((showAny(x._1), showAny(x._2), showAny(x._3)))
  }

  def main(args: Array[String]): Unit = {
    val x1 = (Iterator(1), Iterator("a"), Iterator(Array(1))).invert
    val x2 = (Array(1), Array("a"), Array(Array(1))).invert
    val x3 = (List(1), List("a"), List(Array(1))).invert
    val x4 = (Seq(1), List("a"), Vector(Array(1))).invert
    val x5 = (Vector(1), List("a"), Seq(Array(1))).invert
    val x6 = (Stream from 1, Stream from 2, Stream from 3).invert
    val x7 = ("abc", "def", "ghi").invert
    val x8 = (1 to 10 par, 1 to 10 par, 1 to 10 par).invert

    List[GenTraversableOnce[(_, _, _)]](x1, x2, x3, x4, x5, x6, x7, x8) foreach (x => println(show(x.toIterator.next)))
  }
}
