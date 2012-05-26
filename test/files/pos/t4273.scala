class A {
  class Bippy
  implicit val bippyOrdering = new Ordering[Bippy] { def compare(x: Bippy, y: Bippy) = util.Random.nextInt }
  
  (new Bippy) < (new Bippy)
}