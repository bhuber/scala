/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package util

import scala.tools.nsc._

/** A tool for identifying which classfile is being used.
 *  under the given conditions.
 */
object Which {
  def main(args: Array[String]): Unit = {
    val settings = new Settings()
    val names = settings.processArguments(args.toList, true)._2
    val global = new Global(settings)
    val cp = global.classPath

    import cp._

    for (name <- names) {
      cp classRep name match {
        case classRep if classRep.hasBinary => println("%s is %s".format(name, classRep.binary))
        case _                              => println("Could not find: %s".format(name))
      }
    }
  }
}




