/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.runtime

import scala.reflect.{ ClassTag, classTag }

class RichClass[T](val clazz: Class[T]) {
  def toTag: ClassTag[T] = ClassTag[T](clazz)
  // def toTypeString: String = TypeStrings.fromClazz(clazz)

  // Sadly isAnonymousClass does not return true for scala anonymous
  // classes because our naming scheme is not doing well against the
  // jvm's many assumptions.
  def isScalaAnonymous = (
    try clazz.isAnonymousClass || (clazz.getName contains "$anon$")
    catch { case _: java.lang.InternalError => false }  // good ol' "Malformed class name"
  )

  /** It's not easy... to be... me... */
  def supermans: List[ClassTag[_]] = supers map (_.toTag)
  def superNames: List[String]    = supers map (_.getName)
  def interfaces: List[Class[_]]    = supers filter (_.isInterface)

  def hasAncestorName(f: String => Boolean) = superNames exists f
  def hasAncestor(f: Class[_] => Boolean) = supers exists f
  def hasAncestorInPackage(pkg: String) = hasAncestorName(_ startsWith (pkg + "."))

  def supers: List[Class[_]] = {
    def loop(x: Class[_]): List[Class[_]] = x.getSuperclass match {
      case null   => List(x)
      case sc     => x :: (x.getInterfaces.toList flatMap loop) ++ loop(sc)
    }
    loop(clazz).distinct
  }
}
