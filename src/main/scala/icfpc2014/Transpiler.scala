package icfpc2014

import java.io.File

object Transpiler extends App {
  def solveIncludes(path: String, src: Iterator[String]): Iterator[String] = {
    val (header, body) = src.span(_.startsWith("(include"))
    header.flatMap { f => solveIncludes(path, io.Source.fromFile(path + f.split(' ')(1).dropRight(1)).getLines()) } ++ body
  }

  val source = if (args.length == 1) {
    val mainFile = new File(args(0))
    val path = mainFile.getParent + "/"
    "(progn " + solveIncludes(path, io.Source.fromFile(mainFile).getLines()).map(_.takeWhile(_ != ';')).mkString("\n") + ")"
  } else io.Source.stdin.getLines().map(_.takeWhile(_ != ';')).mkString("\n")

  println(Program(new Compiler(source).compile))
}