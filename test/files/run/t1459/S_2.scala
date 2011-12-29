class ScalaConcrete extends Ab_1[java.lang.Integer] {
  override def doStuff(params: java.lang.Integer*) { }
  def doOne(param: java.lang.Integer) { }
}

object Test {
  def main(args: Array[String]) {
    val impl = new ScalaConcrete
    impl.callDoStuff(1,2,3)
  }
}
