trait FoldRight[-M[_]]

object Test {
  def f1 = {
    implicit val ListFoldRight: FoldRight[List] = new FoldRight[List] { override def toString = "ListFoldRight" }
    implicit val IterableFoldRight: FoldRight[Iterable] = new FoldRight[Iterable] { override def toString = "IterableFoldRight" }

    println(implicitly[FoldRight[List]])
    println(implicitly[FoldRight[Seq]])
    println(implicitly[FoldRight[Iterable]])
  }
  def f2 = {
    abstract class Low {
      implicit val IterableFoldRight: FoldRight[Iterable] = new FoldRight[Iterable] { override def toString = "IterableFoldRight" }
    }
    object FoldRight extends Low {
      implicit val ListFoldRight: FoldRight[List] = new FoldRight[List] { override def toString = "ListFoldRight" }
    }
    import FoldRight._

    println(implicitly[FoldRight[List]])
    println(implicitly[FoldRight[Seq]])
    println(implicitly[FoldRight[Iterable]])
  }

  def main(args: Array[String]): Unit = {
    f1
    f2
  }
}
