package icfpc2014

object Transpiler extends App {
  val program = io.Source.stdin.getLines().mkString("\n")
  println(Program(new Compiler(program).compile))
}