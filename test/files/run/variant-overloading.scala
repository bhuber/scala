trait Ord1[-T]
trait Ord2[-T] extends Ord1[T]
trait Bag1[+T]
trait Bag2[+T] extends Bag1[T]

object Test {
  def f(x: Ord1[AnyRef]): Ord1[AnyRef] = { print("1") ; null }
  def f(x: Ord1[String]): Ord2[AnyRef] = { print("2") ; null }
  def f(x: Ord2[AnyRef]): Ord1[AnyRef] = { print("3") ; null }
  def f(x: Ord2[String]): Ord2[AnyRef] = { print("4") ; null }

  def f2(x: Ord1[AnyRef]): Ord1[String] = { print("1") ; null }
  def f2(x: Ord1[String]): Ord2[String] = { print("2") ; null }
  def f2(x: Ord2[AnyRef]): Ord1[String] = { print("3") ; null }
  def f2(x: Ord2[String]): Ord2[String] = { print("4") ; null }

  def f3(x: Ord1[String]): Ord1[AnyRef] = { print("1") ; null }
  def f3(x: Ord1[AnyRef]): Ord2[AnyRef] = { print("2") ; null }
  def f3(x: Ord2[String]): Ord1[AnyRef] = { print("3") ; null }
  def f3(x: Ord2[AnyRef]): Ord2[AnyRef] = { print("4") ; null }

  def f4(x: Ord1[String]): Ord1[String] = { print("1") ; null }
  def f4(x: Ord1[AnyRef]): Ord2[String] = { print("2") ; null }
  def f4(x: Ord2[String]): Ord1[String] = { print("3") ; null }
  def f4(x: Ord2[AnyRef]): Ord2[String] = { print("4") ; null }

  def g(x: Bag1[AnyRef]): Bag1[AnyRef] = { print("1") ; null }
  def g(x: Bag1[String]): Bag2[AnyRef] = { print("2") ; null }
  def g(x: Bag2[AnyRef]): Bag1[AnyRef] = { print("3") ; null }
  def g(x: Bag2[String]): Bag2[AnyRef] = { print("4") ; null }

  def g2(x: Bag1[AnyRef]): Bag1[String] = { print("1") ; null }
  def g2(x: Bag1[String]): Bag2[String] = { print("2") ; null }
  def g2(x: Bag2[AnyRef]): Bag1[String] = { print("3") ; null }
  def g2(x: Bag2[String]): Bag2[String] = { print("4") ; null }

  def g3(x: Bag1[String]): Bag1[AnyRef] = { print("1") ; null }
  def g3(x: Bag1[AnyRef]): Bag2[AnyRef] = { print("2") ; null }
  def g3(x: Bag2[String]): Bag1[AnyRef] = { print("3") ; null }
  def g3(x: Bag2[AnyRef]): Bag2[AnyRef] = { print("4") ; null }

  def g4(x: Bag1[String]): Bag1[String] = { print("1") ; null }
  def g4(x: Bag1[AnyRef]): Bag2[String] = { print("2") ; null }
  def g4(x: Bag2[String]): Bag1[String] = { print("3") ; null }
  def g4(x: Bag2[AnyRef]): Bag2[String] = { print("4") ; null }

  def main(args: Array[String]): Unit = {
    f(null: Ord1[AnyRef])
    f(null: Ord1[String])
    f(null: Ord2[AnyRef])
    f(null: Ord2[String])
    (f(null: Ord1[AnyRef]): Ord1[_])
    (f(null: Ord1[String]): Ord1[_])
    (f(null: Ord2[AnyRef]): Ord1[_])
    (f(null: Ord2[String]): Ord1[_])
    (f(null: Ord1[AnyRef]): Ord2[_])
    (f(null: Ord1[String]): Ord2[_])
    (f(null: Ord2[AnyRef]): Ord2[_])
    (f(null: Ord2[String]): Ord2[_])
    println("")

    f2(null: Ord1[AnyRef])
    f2(null: Ord1[String])
    f2(null: Ord2[AnyRef])
    f2(null: Ord2[String])
    (f2(null: Ord1[AnyRef]): Ord1[_])
    (f2(null: Ord1[String]): Ord1[_])
    (f2(null: Ord2[AnyRef]): Ord1[_])
    (f2(null: Ord2[String]): Ord1[_])
    (f2(null: Ord1[AnyRef]): Ord2[_])
    (f2(null: Ord1[String]): Ord2[_])
    (f2(null: Ord2[AnyRef]): Ord2[_])
    (f2(null: Ord2[String]): Ord2[_])
    println("")

    f3(null: Ord1[AnyRef])
    f3(null: Ord1[String])
    f3(null: Ord2[AnyRef])
    f3(null: Ord2[String])
    (f3(null: Ord1[AnyRef]): Ord1[_])
    (f3(null: Ord1[String]): Ord1[_])
    (f3(null: Ord2[AnyRef]): Ord1[_])
    (f3(null: Ord2[String]): Ord1[_])
    (f3(null: Ord1[AnyRef]): Ord2[_])
    print("X") // (f3(null: Ord1[String]): Ord2[_])
    (f3(null: Ord2[AnyRef]): Ord2[_])
    print("X") // (f3(null: Ord2[String]): Ord2[_])
    println("")

    f4(null: Ord1[AnyRef])
    f4(null: Ord1[String])
    f4(null: Ord2[AnyRef])
    f4(null: Ord2[String])
    (f4(null: Ord1[AnyRef]): Ord1[_])
    (f4(null: Ord1[String]): Ord1[_])
    (f4(null: Ord2[AnyRef]): Ord1[_])
    (f4(null: Ord2[String]): Ord1[_])
    (f4(null: Ord1[AnyRef]): Ord2[_])
    print("X") // (f4(null: Ord1[String]): Ord2[_])
    (f4(null: Ord2[AnyRef]): Ord2[_])
    print("X") // (f4(null: Ord2[String]): Ord2[_])
    println("")

    g(null: Bag1[AnyRef])
    g(null: Bag1[String])
    g(null: Bag2[AnyRef])
    g(null: Bag2[String])
    (g(null: Bag1[AnyRef]): Bag1[_])
    (g(null: Bag1[String]): Bag1[_])
    (g(null: Bag2[AnyRef]): Bag1[_])
    (g(null: Bag2[String]): Bag1[_])
    print("X") // (g(null: Bag1[AnyRef]): Bag2[_])
    (g(null: Bag1[String]): Bag2[_])
    print("X") // (g(null: Bag2[AnyRef]): Bag2[_])
    (g(null: Bag2[String]): Bag2[_])
    println("")

    g2(null: Bag1[AnyRef])
    g2(null: Bag1[String])
    g2(null: Bag2[AnyRef])
    g2(null: Bag2[String])
    (g2(null: Bag1[AnyRef]): Bag1[_])
    (g2(null: Bag1[String]): Bag1[_])
    (g2(null: Bag2[AnyRef]): Bag1[_])
    (g2(null: Bag2[String]): Bag1[_])
    print("X") // (g2(null: Bag1[AnyRef]): Bag2[_])
    (g2(null: Bag1[String]): Bag2[_])
    print("X") // (g2(null: Bag2[AnyRef]): Bag2[_])
    (g2(null: Bag2[String]): Bag2[_])
    println("")

    g3(null: Bag1[AnyRef])
    g3(null: Bag1[String])
    g3(null: Bag2[AnyRef])
    g3(null: Bag2[String])
    (g3(null: Bag1[AnyRef]): Bag1[_])
    (g3(null: Bag1[String]): Bag1[_])
    (g3(null: Bag2[AnyRef]): Bag1[_])
    (g3(null: Bag2[String]): Bag1[_])
    (g3(null: Bag1[AnyRef]): Bag2[_])
    (g3(null: Bag1[String]): Bag2[_])
    (g3(null: Bag2[AnyRef]): Bag2[_])
    (g3(null: Bag2[String]): Bag2[_])
    println("")

    g4(null: Bag1[AnyRef])
    g4(null: Bag1[String])
    g4(null: Bag2[AnyRef])
    g4(null: Bag2[String])
    (g4(null: Bag1[AnyRef]): Bag1[_])
    (g4(null: Bag1[String]): Bag1[_])
    (g4(null: Bag2[AnyRef]): Bag1[_])
    (g4(null: Bag2[String]): Bag1[_])
    (g4(null: Bag1[AnyRef]): Bag2[_])
    (g4(null: Bag1[String]): Bag2[_])
    (g4(null: Bag2[AnyRef]): Bag2[_])
    (g4(null: Bag2[String]): Bag2[_])
    println("")
  }
}
