package icfpc2014

import java.io.File
import scala.annotation.tailrec
import scala.io.Source

object Transpiler extends App {
  def tsort[A](edges: Traversable[(A, A)]): Iterable[A] = {
    @tailrec
    def tsort(toPreds: Map[A, Set[A]], done: Iterable[A]): Iterable[A] = {
      val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty) done else sys.error(hasPreds.toString)
      } else {
        val found = noPreds.map { _._1 }
        tsort(hasPreds.mapValues { _ -- found }, done ++ found)
      }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
      acc + (e._1 -> acc.getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }
    tsort(toPred, Seq())
  }

  def getSourceBody(path: String, sources: Iterable[String]): Iterator[String] =
    sources.toIterator.flatMap { i =>
      Source.fromFile(path + i).getLines().span(_.startsWith("(include"))._2
    }

  def buildIncludeGraph(current: String, path: String, src: Iterator[String]): Set[(String, String)] = {
    val (headers, body) = src.span(_.startsWith("(include"))
    val hs = headers.map(_.split(' ')(1).dropRight(1)).toSet

    hs.map(h => (h -> current)) ++
    hs.map(h => buildIncludeGraph(h, path, Source.fromFile(path + h).getLines())).flatten
  }

  val source = if (args.length == 1) {
    val mainFile = new File(args(0))
    val path = mainFile.getParent + "/"
    val includes = tsort(buildIncludeGraph(args(0).split('/').last, path, Source.fromFile(mainFile).getLines()))
    "(progn " + getSourceBody(path, includes).map(_.takeWhile(_ != ';')).mkString("\n") + ")"
  } else Source.stdin.getLines().map(_.takeWhile(_ != ';')).mkString("\n")

  println(Program(new Compiler(source).compile))
}
