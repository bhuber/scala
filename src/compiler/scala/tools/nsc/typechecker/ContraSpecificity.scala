/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

trait ContraSpecificity extends Analyzer {
  import global._

  // This is the spot where we rescue contravariance from uselessness.
  // A simple subtype check will consider Ordering[Any] to be more
  // specific than Ordering[MyVerySpecificType]. Since this defeats the
  // purpose of being able to specialize behavior via subclassing, we
  // should instead judge specificity based on the deepest subtype, even
  // for contravariant type parameters. This of course assumes that both
  // candidate types are already known to be sound inferences; the only
  // question is which to infer.
  override def isAsSpecificValueType(infer: Inferencer, tpe1: Type, tpe2: Type, undef1: List[Symbol], undef2: List[Symbol]) = {
    def standardResult = super.isAsSpecificValueType(infer, tpe1, tpe2, undef1, undef2)

    (tpe1, tpe2) match {
      case (t1: TypeRef, t2: TypeRef) =>
        val ntpe1 @ TypeRef(pre1, sym1, args1) = tpe1.dealias
        val ntpe2 @ TypeRef(pre2, sym2, args2) = tpe2.dealias
        def subtypeResult = existentialAbstraction(undef1, tpe1) <:< existentialAbstraction(undef2, tpe2)

        (sym1 isSubClass sym2) && {
          val t1 = ntpe1 baseType sym2
          val t2 = ntpe2
          val ps = t1.typeSymbol.typeParams

          if (ps exists (_.isContravariant)) {
            corresponds3(ps, t1.typeArgs, t2.typeArgs) { (p, x, y) =>
              val res = x <:< y
              if (p.isContravariant && res != (y <:< x)) {
                log("Specificity reversal comparing args of %s and %s, '%s': %s is%s as specific as %s in %s".format(
                  t1, t2, t1.typeSymbol.defString, x, if (res) "    " else " not", y, p.defString))
              }
              res
            }
          }
          else subtypeResult
        }
      case _ => super.isAsSpecificValueType(infer, tpe1, tpe2, undef1, undef2)
    }
  }
}
