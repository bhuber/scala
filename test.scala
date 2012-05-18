package bippy

abstract class AbstractTraversable[+A] extends Traversable[A]
abstract class AbstractIterable[+A] extends AbstractTraversable[A] with Iterable[A]
abstract class AbstractSet[A] extends AbstractIterable[A] with Set[A]
