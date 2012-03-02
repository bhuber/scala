#!/usr/bin/env bash
  exec scala -nc "$0" "$@"
!#

import scala.tools.nsc.io.{ Directory, File }
import java.io.{ ByteArrayOutputStream, OutputStream, PrintStream }
import scala.sys.process._
import scala.tools.nsc.util.ScalaClassLoader

if (args.length < 3) {
  println("Usage: runner <category> <testdir> <workdir> [test test ...]")
  sys.exit(1)
}
val category = args(0)                          // e.g. run
val testdir = Directory(args(1)).toAbsolute     // e.g. ./files/run
val workdir = Directory(args(2)).toAbsolute     // e.g. /var/tmp/DFSLKFJASDLKFJ/run
val outdir = workdir / "target" toDirectory     // where the classfiles are
val skipped = workdir / "skipped.txt" toFile    // for sending failures back to partest
val tests = args drop 3

if (!testdir.isDirectory || !outdir.isDirectory) {
  println("The testdir and outdir must be directories.")
  println("Arguments received: " + args.mkString(" "))
  sys.exit(1)
}
if (!skipped.isFile) {
  println(skipped + " must exist.")
  sys.exit(1)
}
def baseTestName(s: String) = (s split '/').last stripSuffix ".scala"
// From test-foo-5.scala to run_testfoo5
// This links the original test with the package holding its classfile
def encode(s: String) = {
  val str = baseTestName(s) filter (x => x.isLetter || x.isDigit)
  category + "_" + str
}
def checkFileContents(test: String): Option[String] = {
  val checkFile = testdir / (baseTestName(test) + ".check") toFile;

  if (checkFile.isFile) Some(checkFile.slurp())
  else None
}

val loader = ScalaClassLoader.fromURLs(Seq(outdir.toURL))
var failedTests = List[String]()
var failures = 0
var successes = 0

def stringFromStream(stream: OutputStream => Unit): String = {
  val bs = new ByteArrayOutputStream()
  val ps = new PrintStream(bs)
  stream(ps)
  ps.close()
  bs.toString()
}

def runner(program: String): (Boolean, String) = {
  var threw = false
  val output = stringFromStream { ostream =>
    Console.withOut(ostream) {
      try   { loader.run(program, Array("jvm")) }
      catch { case x => threw = true ; println("Exception running " + program + ": " + x.getClass + " " + x) }
    }
  }
  (!threw, output)
}

def printResult(test: String, ok: Boolean, note: String = "") {
  val totalWidth = 56
  val path       = "files/" + category + "/" + (test split '/' last)
  val str        = "[...]%s%s".format(path, " " * (totalWidth - path.length))
  val outcome    = if (ok) "  OK  " else "FAILED"

  println("testing: " + str + "[" + outcome + "]" + note)
}
def logFailure(path: String, output: String) {
  failures += 1
  failedTests ::= path
  val logFile = workdir / (baseTestName(path) + "-" + category + ".log") toFile;
  logFile writeAll output

}
def logSuccess(path: String, msg: String) {
  successes += 1
  printResult(path, true, msg)
}

tests foreach { test =>
  val runnerPackage = encode(test)
  val (ok, output)  = runner(runnerPackage + ".Test")

  if (!ok) logFailure(test, output)
  else checkFileContents(test) match {
    case Some(`output`) => logSuccess(test, " " + output.length + " chars matched")
    case None           => logSuccess(test, " no checkfile")
    case Some(content)  => logFailure(test, output)
  }
}

if (failedTests.isEmpty) {
  println("[%s] All %d tests passed.".format(category, successes))
}
else if (failedTests.nonEmpty) {
  val xs = failedTests.sorted
  println("[%s] kicking %d tests back to partest: %s".format(category, failures, failedTests.mkString(" ")))
  skipped appendAll failedTests.map(_ + "\n").mkString("")
}

sys.exit(0)
