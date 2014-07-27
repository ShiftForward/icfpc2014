package icfpc2014

import scala.io.Source

object preprocessor extends App {
  /**
   * Example:
   * jgt %left,[c],[e]
   * mov a,2
   * left: mov a,3
   */
  def findLabels(lines: List[String]): (Map[String, Int], List[String]) = {
    val regex = """^(\w+):\s(.*)""".r
    lines.zipWithIndex.foldRight(Map[String, Int](), List[String]()) { case ((line, i), (labels, lines)) =>
      line match {
        case regex(label, line) => (labels + (label -> i), line :: lines)
        case _ => (labels, line :: lines)
      }
    }
  }

  def removeEmptyLines(lines: List[String]) =
    lines.map(_.trim).filterNot(l => l.isEmpty || l.startsWith(";"))

  def setAddresses(lines: List[String], labels: Map[String, Int]): List[String] = {
    val regex = """(.*)%(\w+)(.*)""".r
    lines.foldRight(List[String]()) { case (l, acc) =>
      l match {
        case regex(head, label, rest) =>
          labels.get(label) match {
            case Some(address) => (head + address + rest) :: acc
            case None => throw new Exception(s"Undefined label $label")
          }
        case _ => l :: acc
      }
    }
  }

  val source =
    if (args.length == 1) Source.fromFile(args(0)).getLines.toList
    else Source.stdin.getLines.toList

  val code = removeEmptyLines(source)

  val (labels, lines) = findLabels(code)
  println(setAddresses(lines, labels).map(_.trim).mkString("\n"))
}
