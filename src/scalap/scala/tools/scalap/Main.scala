/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2011, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
**
*/

package scala.tools.scalap

import java.io.{ PrintStream, OutputStreamWriter, ByteArrayOutputStream }
import scala.reflect.NameTransformer
import scalax.rules.scalasig._
import scala.io.ClassProvision._
import tools.nsc.util.JavaClassPath
import tools.nsc.io.{ PlainFile, AbstractFile }

/**The main object used to execute scalap on the command-line.
 *
 * @author Matthias Zenger, Stephane Micheloud, Burak Emir, Ilya Sergey
 */
class Main {
  val SCALA_SIG            = "ScalaSig"
  val SCALA_SIG_ANNOTATION = "Lscala/reflect/ScalaSignature;"
  val BYTES_VALUE          = "bytes"

  val versionMsg = "Scala classfile decoder %s -- %s\n".format(Properties.versionString, Properties.copyrightString)

  /**Verbose program run?
   */
  var verbose = false
  var printPrivates = false

  def isScalaFile(bytes: Array[Byte]): Boolean = {
    val byteCode  = ByteCode(bytes)
    val classFile = ClassFileParser.parse(byteCode)
    classFile.attribute("ScalaSig").isDefined
  }

  /**Processes the given Java class file.
   *
   * @param clazz the class file to be processed.
   */
  def processJavaClassFile(clazz: Classfile) {
    // construct a new output stream writer
    val out = new OutputStreamWriter(Console.out)
    val writer = new JavaWriter(clazz, out)
    // print the class
    writer.printClass
    out.flush()
  }

  def isPackageObjectFile(s: String) = s != null && (s.endsWith(".package") || s == "package")

  def parseScalaSignature(scalaSig: ScalaSig, isPackageObject: Boolean) = {
    val baos   = new ByteArrayOutputStream
    val stream = new PrintStream(baos)
    val syms   = scalaSig.topLevelClasses ++ scalaSig.topLevelObjects

    syms.head.parent match {
      // Partial match
      case Some(p) if (p.name != "<empty>") => {
        val path = p.path
        if (!isPackageObject) {
          stream.print("package ");
          stream.print(path);
          stream.print("\n")
        } else {
          val i = path.lastIndexOf(".")
          if (i > 0) {
            stream.print("package ");
            stream.print(path.substring(0, i))
            stream.print("\n")
          }
        }
      }
      case _ =>
    }
    // Print classes
    val printer = new ScalaSigPrinter(stream, printPrivates)
    syms foreach (printer printSymbol _)
    baos.toString
  }

  def decompileScala(bytes: Array[Byte], isPackageObject: Boolean): String = {
    val byteCode = ByteCode(bytes)
    val classFile = ClassFileParser.parse(byteCode)

    ScalaSigParser.parse(classFile) match {
      case Some(scalaSig) => parseScalaSignature(scalaSig, isPackageObject)
      case None           => ""
    }
  }

  /** Executes scalap with the given arguments and classpath for the
   *  class denoted by `classname`.
   */
  def process(args: Arguments, provider: ClassRepProvider)(classname: String): Unit = {
    val name    = if (classname == "scala.AnyRef") "java.lang.Object" else classname
    val encName = NameTransformer.encode(name)

    // find the classfile
    provider.classRep(encName).binary match {
      case NoClassBinaryFile => Console.println("class/object " + classname + " not found.")
      case cfile        =>
        if (verbose)
          Console.println(Console.BOLD + "FILENAME" + Console.RESET + " = " + cfile)

        val bytes = cfile.bytes
        if (isScalaFile(bytes))
          Console.println(decompileScala(bytes, isPackageObjectFile(encName)))
        else {
          // parse the classfile
          val clazz = new Classfile(new ByteArrayReader(bytes))
          processJavaClassFile(clazz)
        }
    }
  }
}

object Main extends Main {
  /** Prints usage information for scalap. */
  def usage() {
    Console println """
      |Usage: scalap {<option>} <name>
      |where <name> is fully-qualified class name or <package_name>.package for package objects
      |and <option> is
      |  -private           print private definitions
      |  -verbose           print out additional information
      |  -version           print out the version number of scalap
      |  -help              display this usage message
      |  -classpath <path>  specify where to find user class files
      |  -cp <path>         specify where to find user class files
    """.stripMargin.trim
  }

  def main(args: Array[String]) {
    // print usage information if there is no command-line argument
    if (args.isEmpty)
      return usage()

    val arguments = Arguments.Parser('-')
            .withOption("-private")
            .withOption("-verbose")
            .withOption("-version")
            .withOption("-help")
            .withOptionalArg("-classpath")
            .withOptionalArg("-cp")
            .parse(args);

    if (arguments contains "-version")
      Console.println(versionMsg)
    if (arguments contains "-help")
      usage()

    verbose       = arguments contains "-verbose"
    printPrivates = arguments contains "-private"
    // construct a custom class path
    val cparg = List("-classpath", "-cp") map (arguments getArgument _) reduceLeft (_ orElse _)
    val path  = JavaClassPath fromClasspath (cparg getOrElse ".")
    // print the classpath if output is verbose
    if (verbose)
      Console.println(Console.BOLD + "CLASSPATH" + Console.RESET + " = " + path)

    // process all given classes
    arguments.getOthers foreach process(arguments, path)
  }
}
