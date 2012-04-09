object Test {
  def f[T: ArrayManifest](size: Int) = new Array[T](size)
  def g[T: ClassManifest](size: Int) = new Array[T](size)

  def check[T](implicit m: ArrayManifest[T] = null) = println("m = " + m)

  def arr1 () {
    val xs = f[Double](50)
    val ys = f[Float](50)

    ys.indices foreach (i => ys(i) = 1.0f)
    xs.indices foreach (i => xs(i) = ys(i))
    println((xs.sum, ys.sum))
  }
  def arr2 () {
    val xs = g[Double](50)
    val ys = g[Float](50)

    ys.indices foreach (i => ys(i) = 1.0f)
    xs.indices foreach (i => xs(i) = ys(i))
    println((xs.sum, ys.sum))
  }
  def arr3 () {
    implicit val myArrayManifest = new ArrayManifest[List[String]] {
      def newArray(len: Int): Array[List[String]] = new Array[List[String]](len)
      def newArray2(len: Int): Array[Array[List[String]]] = ???
      def newArray3(len: Int): Array[Array[Array[List[String]]]] = ???
      def newArray4(len: Int): Array[Array[Array[Array[List[String]]]]] = ???
      def newArray5(len: Int): Array[Array[Array[Array[Array[List[String]]]]]] = ???
    }

    val xs = f[List[String]](10)
    xs.indices foreach (i => xs(i) = List.fill(i)("x"))

    xs map (_ mkString) foreach println
  }

  def main(args: Array[String]): Unit = {
    arr1()
    arr2()
    check[ArrayManifest[List[String]]]
    arr3()
  }
}
