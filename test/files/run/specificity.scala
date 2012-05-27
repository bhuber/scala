class C1
class C2 extends C1
class C3 extends C2

trait MInv1[T]
trait MInv2[T] extends MInv1[T]
trait MInv3[T] extends MInv2[T]

object MInv1 { def apply[T] = new MInv1[T] { } ; override def toString = "MInv1" }
object MInv2 { def apply[T] = new MInv2[T] { } ; override def toString = "MInv2" }
object MInv3 { def apply[T] = new MInv3[T] { } ; override def toString = "MInv3" }

trait MCov1[+T]
trait MCov2[+T] extends MCov1[T]
trait MCov3[+T] extends MCov2[T]

object MCov1 { def apply[T] = new MCov1[T] { } ; override def toString = "MCov1" }
object MCov2 { def apply[T] = new MCov2[T] { } ; override def toString = "MCov2" }
object MCov3 { def apply[T] = new MCov3[T] { } ; override def toString = "MCov3" }

trait MCon1[-T]
trait MCon2[-T] extends MCon1[T]
trait MCon3[-T] extends MCon2[T]

object MCon1 { def apply[T] = new MCon1[T] { } ; override def toString = "MCon1" }
object MCon2 { def apply[T] = new MCon2[T] { } ; override def toString = "MCon2" }
object MCon3 { def apply[T] = new MCon3[T] { } ; override def toString = "MCon3" }

trait TInv1[M[_]]
trait TInv2[M[_]] extends TInv1[M]
trait TInv3[M[_]] extends TInv2[M]

object TInv1 { def apply[M[_]] = new TInv1[M] { } ; override def toString = "TInv1" }
object TInv2 { def apply[M[_]] = new TInv2[M] { } ; override def toString = "TInv2" }
object TInv3 { def apply[M[_]] = new TInv3[M] { } ; override def toString = "TInv3" }

trait TCov1[+M[_]]
trait TCov2[+M[_]] extends TCov1[M]
trait TCov3[+M[_]] extends TCov2[M]

object TCov1 { def apply[M[_]] = new TCov1[M] { } ; override def toString = "TCov1" }
object TCov2 { def apply[M[_]] = new TCov2[M] { } ; override def toString = "TCov2" }
object TCov3 { def apply[M[_]] = new TCov3[M] { } ; override def toString = "TCov3" }

trait TCon1[-M[_]]
trait TCon2[-M[_]] extends TCon1[M]
trait TCon3[-M[_]] extends TCon2[M]

object TCon1 { def apply[M[_]] = new TCon1[M] { } ; override def toString = "TCon1" }
object TCon2 { def apply[M[_]] = new TCon2[M] { } ; override def toString = "TCon2" }
object TCon3 { def apply[M[_]] = new TCon3[M] { } ; override def toString = "TCon3" }

object Test {

  class Overload0 {
    def f(x: MCov1[Int]): AnyRef = MCov1
    def f(x: MCov2[Int]): AnyRef = MCov2
    def f(x: MCov3[Int]): AnyRef = MCov3

    println(f(MCov1[Int]))
    println(f(MCov2[Int]))
    println(f(MCov3[Int]))
  }
  class OverloadCov1 {
    def f(x: MCov1[C3]): AnyRef = MCov1
    def f(x: MCov2[C2]): AnyRef = MCov2
    def f(x: MCov3[C1]): AnyRef = MCov3

    // println("f(MCov1[C1]) = " + f(MCov1[C1]))   // 1
    // println("f(MCov1[C2]) = " + f(MCov1[C2]))   // 2
    println("f(MCov1[C3]) = " + f(MCov1[C3]))   // 3
    // println("f(MCov2[C1]) = " + f(MCov2[C1]))   // 4
    println("f(MCov2[C2]) = " + f(MCov2[C2]))   // 5
    // println("f(MCov2[C3]) = " + f(MCov2[C3]))   // 6
    println("f(MCov3[C1]) = " + f(MCov3[C1]))   // 7
    // println("f(MCov3[C2]) = " + f(MCov3[C2]))   // 8
    // println("f(MCov3[C3]) = " + f(MCov3[C3]))   // 9
    println("")
  }
  class OverloadCov2 {
    def f(x: MCov1[C1]): AnyRef = MCov1
    def f(x: MCov2[C2]): AnyRef = MCov2
    def f(x: MCov3[C3]): AnyRef = MCov3

    println("f(MCov1[C1]) = " + f(MCov1[C1]))   // 1
    println("f(MCov1[C2]) = " + f(MCov1[C2]))   // 2
    println("f(MCov1[C3]) = " + f(MCov1[C3]))   // 3
    println("f(MCov2[C1]) = " + f(MCov2[C1]))   // 4
    println("f(MCov2[C2]) = " + f(MCov2[C2]))   // 5
    println("f(MCov2[C3]) = " + f(MCov2[C3]))   // 6
    println("f(MCov3[C1]) = " + f(MCov3[C1]))   // 7
    println("f(MCov3[C2]) = " + f(MCov3[C2]))   // 8
    println("f(MCov3[C3]) = " + f(MCov3[C3]))   // 9
    println("")
  }

  class OverloadInv1 {
    def f(x: MInv1[C3]): AnyRef = MInv1
    def f(x: MInv2[C2]): AnyRef = MInv2
    def f(x: MInv3[C1]): AnyRef = MInv3

    // println("f(MInv1[C1]) = " + f(MInv1[C1]))   // 1
    // println("f(MInv1[C2]) = " + f(MInv1[C2]))   // 2
    println("f(MInv1[C3]) = " + f(MInv1[C3]))   // 3
    // println("f(MInv2[C1]) = " + f(MInv2[C1]))   // 4
    println("f(MInv2[C2]) = " + f(MInv2[C2]))   // 5
    println("f(MInv2[C3]) = " + f(MInv2[C3]))   // 6
    println("f(MInv3[C1]) = " + f(MInv3[C1]))   // 7
    println("f(MInv3[C2]) = " + f(MInv3[C2]))   // 8
    println("f(MInv3[C3]) = " + f(MInv3[C3]))   // 9
    println("")
  }
  class OverloadInv2 {
    def f(x: MInv1[C1]): AnyRef = MInv1
    def f(x: MInv2[C2]): AnyRef = MInv2
    def f(x: MInv3[C3]): AnyRef = MInv3

    println("f(MInv1[C1]) = " + f(MInv1[C1]))   // 1
    // println("f(MInv1[C2]) = " + f(MInv1[C2]))   // 2
    // println("f(MInv1[C3]) = " + f(MInv1[C3]))   // 3
    println("f(MInv2[C1]) = " + f(MInv2[C1]))   // 4
    println("f(MInv2[C2]) = " + f(MInv2[C2]))   // 5
    // println("f(MInv2[C3]) = " + f(MInv2[C3]))   // 6
    println("f(MInv3[C1]) = " + f(MInv3[C1]))   // 7
    println("f(MInv3[C2]) = " + f(MInv3[C2]))   // 8
    println("f(MInv3[C3]) = " + f(MInv3[C3]))   // 9
    println("")
  }

  class OverloadCon1 {
    def f(x: MCon1[C3]): AnyRef = MCon1
    def f(x: MCon2[C2]): AnyRef = MCon2
    def f(x: MCon3[C1]): AnyRef = MCon3

    println("f(MCon1[C1]) = " + f(MCon1[C1]))   // 1
    println("f(MCon1[C2]) = " + f(MCon1[C2]))   // 2
    println("f(MCon1[C3]) = " + f(MCon1[C3]))   // 3
    println("f(MCon2[C1]) = " + f(MCon2[C1]))   // 4
    println("f(MCon2[C2]) = " + f(MCon2[C2]))   // 5
    println("f(MCon2[C3]) = " + f(MCon2[C3]))   // 6
    println("f(MCon3[C1]) = " + f(MCon3[C1]))   // 7
    println("f(MCon3[C2]) = " + f(MCon3[C2]))   // 8
    println("f(MCon3[C3]) = " + f(MCon3[C3]))   // 9
    println("")
  }
  class OverloadCon2 {
    def f(x: MCon1[C1]): AnyRef = MCon1
    def f(x: MCon2[C2]): AnyRef = MCon2
    def f(x: MCon3[C3]): AnyRef = MCon3

    println("f(MCon1[C1]) = " + f(MCon1[C1]))   // 1
    // println("f(MCon1[C2]) = " + f(MCon1[C2]))   // 2
    // println("f(MCon1[C3]) = " + f(MCon1[C3]))   // 3
    // println("f(MCon2[C1]) = " + f(MCon2[C1]))   // 4
    println("f(MCon2[C2]) = " + f(MCon2[C2]))   // 5
    // println("f(MCon2[C3]) = " + f(MCon2[C3]))   // 6
    // println("f(MCon3[C1]) = " + f(MCon3[C1]))   // 7
    // println("f(MCon3[C2]) = " + f(MCon3[C2]))   // 8
    println("f(MCon3[C3]) = " + f(MCon3[C3]))   // 9
    println("")
  }

  class ImplicitCov1 {
    implicit val TC1: TCov3[MCov1] = new TCov3[MCov1] { override def toString = "TCov3[MCov1]" }
    implicit val TC2: TCov3[MCov2] = new TCov3[MCov2] { override def toString = "TCov3[MCov2]" }
    implicit val TC3: TCov3[MCov3] = new TCov3[MCov3] { override def toString = "TCov3[MCov3]" }

    println(implicitly[TCov1[MCov1]])
    println(implicitly[TCov1[MCov2]])
    println(implicitly[TCov1[MCov3]])
    println(implicitly[TCov2[MCov1]])
    println(implicitly[TCov2[MCov2]])
    println(implicitly[TCov2[MCov3]])
    println(implicitly[TCov3[MCov1]])
    println(implicitly[TCov3[MCov2]])
    println(implicitly[TCov3[MCov3]])
    println("")
  }
  class ImplicitInv1 {
    implicit val TC1: TInv3[MInv1] = new TInv3[MInv1] { override def toString = "TInv3[MInv1]" }
    implicit val TC2: TInv3[MInv2] = new TInv3[MInv2] { override def toString = "TInv3[MInv2]" }
    implicit val TC3: TInv3[MInv3] = new TInv3[MInv3] { override def toString = "TInv3[MInv3]" }

    println(implicitly[TInv1[MInv1]])
    println(implicitly[TInv1[MInv2]])
    println(implicitly[TInv1[MInv3]])
    println(implicitly[TInv2[MInv1]])
    println(implicitly[TInv2[MInv2]])
    println(implicitly[TInv2[MInv3]])
    println(implicitly[TInv3[MInv1]])
    println(implicitly[TInv3[MInv2]])
    println(implicitly[TInv3[MInv3]])
    println("")
  }
  class ImplicitCon1 {
    implicit val TC1: TCon3[MCon1] = new TCon3[MCon1] { override def toString = "TCon3[MCon1]" }
    implicit val TC2: TCon3[MCon2] = new TCon3[MCon2] { override def toString = "TCon3[MCon2]" }
    implicit val TC3: TCon3[MCon3] = new TCon3[MCon3] { override def toString = "TCon3[MCon3]" }

    println(implicitly[TCon1[MCon1]])
    println(implicitly[TCon1[MCon2]])
    println(implicitly[TCon1[MCon3]])
    println(implicitly[TCon2[MCon1]])
    println(implicitly[TCon2[MCon2]])
    println(implicitly[TCon2[MCon3]])
    println(implicitly[TCon3[MCon1]])
    println(implicitly[TCon3[MCon2]])
    println(implicitly[TCon3[MCon3]])
    println("")
  }
  class ImplicitCon2 {
    implicit val TC1: TCon3[MCov1] = new TCon3[MCov1] { override def toString = "TCon3[MCov1]" }
    implicit val TC2: TCon3[MCov2] = new TCon3[MCov2] { override def toString = "TCon3[MCov2]" }
    implicit val TC3: TCon3[MCov3] = new TCon3[MCov3] { override def toString = "TCon3[MCov3]" }

    println(implicitly[TCon1[MCov1]])
    println(implicitly[TCon1[MCov2]])
    println(implicitly[TCon1[MCov3]])
    println(implicitly[TCon2[MCov1]])
    println(implicitly[TCon2[MCov2]])
    println(implicitly[TCon2[MCov3]])
    println(implicitly[TCon3[MCov1]])
    println(implicitly[TCon3[MCov2]])
    println(implicitly[TCon3[MCov3]])
  }

  def main(args: Array[String]): Unit = {
    new OverloadInv1
    new OverloadInv2
    new OverloadCov1
    new OverloadCov2
    new OverloadCon1
    new OverloadCon2

    new ImplicitCov1
    new ImplicitInv1
    new ImplicitCon1
    new ImplicitCon2
  }
}
