package icfpc2014

import java.io.File
import scala.io.Source

object Transpiler extends App {
  def solveIncludes(path: String, src: Iterator[String]): Iterator[String] =
    findIncludes(path, src).toIterator.flatMap { i =>
      Source.fromFile(path + i).getLines().span(_.startsWith("(include"))._2
    } ++ src

  def findIncludes(path: String, src: Iterator[String]): Set[String] = {
    val (headers, body) = src.span(_.startsWith("(include"))
    val hs = headers.map(_.split(' ')(1).dropRight(1)).toSet
    hs ++ hs.map(h => findIncludes(path, Source.fromFile(path + h).getLines())).flatten
  }

  val source = if (args.length == 1) {
    val mainFile = new File(args(0))
    val path = mainFile.getParent + "/"
    "(progn " + solveIncludes(path, Source.fromFile(mainFile).getLines()).map(_.takeWhile(_ != ';')).mkString("\n") + ")"
  } else Source.stdin.getLines().map(_.takeWhile(_ != ';')).mkString("\n")

  println(Program(new Compiler(source).compile))
}
