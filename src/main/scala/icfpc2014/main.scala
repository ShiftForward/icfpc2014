package icfpc2014

sealed trait Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals): (Vector[String], Globals)
}

trait BinaryOp extends Instruction {
  def i1: Instruction
  def i2: Instruction
  def op: String
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (s1, g1) = i1.transpile(pos, locals, globals)
    val (s2, g2) = i2.transpile(pos + s1.length, locals, g1)
    (s1 ++ s2 :+ op, g2)
  }
}

trait UnaryOp extends Instruction {
  def i: Instruction
  def op: String
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (s, g) = i.transpile(pos, locals, globals)
    (s :+ op, g)
  }
}

case class VAR(s: String) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    locals.indexWhere(m =>  m.contains(s)) match {
      case -1 =>
        globals.get(s) match {
          case None =>
            throw new Exception("Unknown variable")
          case Some(i) =>
            (Vector(s"LD ${locals.length} ${i}"), globals)
        }

      case i =>
        if (s == "self")
          (Vector(s"LDF ${locals(i)(s)}"), globals)
        else
          (Vector(s"LD $i ${locals(i)(s)}"), globals)
    }
  }
}

case class CONSTANT(v: Int) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) =
    (Vector(s"LDC $v"), globals)
}

case class ADD(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "ADD"
}

case class SUB(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "SUB"
}

case class MUL(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "MUL"
}

case class DIV(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "DIV"
}

case class EQ(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "CEQ"
}

case class GT(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "CGT"
}

case class GTE(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "CGTE"
}

case class LT(i2: Instruction, i1: Instruction) extends BinaryOp {
  val op = "CGT"
}

case class LTE(i2: Instruction, i1: Instruction) extends BinaryOp {
  val op = "CGTE"
}

case class ATOM(i: Instruction) extends UnaryOp {
  val op = "ATOM"
}

case class CONS(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "CONS"
}

case class CAR(i: CONS) extends UnaryOp {
  val op = "CAR"
}

case class CDR(i: CONS) extends UnaryOp {
  val op = "CDR"
}

case class LET(definitions: (String, Instruction)*)(i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (p, instructions, nextLocals, nextGlobals, _) =
      definitions.foldLeft((pos, Vector[String](), Map[String, Int](), globals, 0)) { case ((pos, instructions, nextLocals, nextGlobals, j), (label, defin)) =>
        val (s, g) = defin.transpile(pos, locals, nextGlobals)
        (pos + s.length, instructions ++ s, nextLocals + (label -> j), g, j + 1)
      }
    val newPos = p + 3
    val (s, g) = i.transpile(newPos, nextLocals :: locals, nextGlobals)
    (instructions ++ Vector(s"LDF $newPos", s"AP ${definitions.length}") ++ Vector("RTN") ++ s, g)
  }
}

case class DEFUN(args: String*)(i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (st, g) = i.transpile(pos + 3, (args.zipWithIndex.toMap + ("self" -> (pos + 3))) :: locals, globals)
    val s = st :+ "RTN"
    (Vector(s"LDF ${pos + 3}", "LDC 1", s"TSEL ${pos + s.length + 3} 0") ++ s, g)
  }
}

case class FUNCALL(label: VAR)(parameters: (Instruction)*) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (p, instructions, nextGlobals) = parameters.foldLeft((pos, Vector[String](), globals)) { case ((pos, instructions, globals), parameter) =>
      val (s, g) = parameter.transpile(pos, locals, globals)
      (pos + s.length, instructions ++ s, g)
    }
    val (s, g) = label.transpile(p, locals, nextGlobals)
    (instructions ++ s :+ s"AP ${parameters.length}", g)
  }
}

case class TFUNCALL(label: VAR)(parameters: (Instruction)*) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (p, instructions, nextGlobals) = parameters.foldLeft((pos, Vector[String](), globals)) { case ((pos, instructions, globals), parameter) =>
      val (s, g) = parameter.transpile(pos, locals, globals)
      (pos + s.length, instructions ++ s, g)
    }
    val (s, g) = label.transpile(p, locals, nextGlobals)
    (instructions ++ s :+ s"TAP ${parameters.length}", g)
  }
}

case class IF(pred: Instruction, thenInst: Instruction, elseInst: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (ps, g1) = pred.transpile(pos, locals, globals)
    val pi = pos + ps.length + 3
    val (tst, g2)  = thenInst.transpile(pi, locals, g1)
    val ts = tst :+ "JOIN"
    val ti = pi + ts.length
    val (est, g3) = elseInst.transpile(ti, locals, g2)
    val es = est :+ "JOIN"
    val ei = ti + es.length
    (ps ++ Vector(s"SEL $pi $ti", "LDC 1", s"TSEL $ei 0") ++ ts ++ es, g3)
  }
}

case class TIF(pred: Instruction, thenInst: Instruction, elseInst: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (ps, g1) = pred.transpile(pos, locals, globals)
    val pi = pos + ps.length + 1
    val (tst, g2)  = thenInst.transpile(pi, locals, g1)
    val ts = tst :+ "RTN"
    val ti = pi + ts.length
    val (est, g3) = elseInst.transpile(ti, locals, g2)
    val es = est :+ "RTN"
    val ei = ti + es.length
    (ps ++ Vector(s"TSEL $pi $ti") ++ ts ++ es, g3)
  }
}

case class OR(i1: Instruction, i2: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) =
    GT(ADD(i1, i2), CONSTANT(0)).transpile(pos, locals, globals)
}

case class AND(i1: Instruction, i2: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) =
    EQ(ADD(i1, i2), CONSTANT(2)).transpile(pos, locals, globals)
}

case class NOT(i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) =
    EQ(i, CONSTANT(0)).transpile(pos, locals, globals)
}

case class PROGN(i: Instruction*) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (_, instructions, nextGlobals) = i.foldLeft((pos, Vector[String]("LDC 0"), globals)) { case ((pos, instructions, globals), instruction) =>
      val (st, g) = instruction.transpile(pos, locals, globals)
      val s = st ++ Vector("CONS", "CDR")
      (pos + s.length, instructions ++ s, g)
    }
    (instructions, nextGlobals)
  }
}

case class DEFVAR(label: String, i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (s, g) = i.transpile(pos, locals, globals)
    val nextGlobal = g + (label -> g.getOrElse(label, g.size))
    (s ++ Vector(s"ST ${locals.length} ${nextGlobal(label)}", s"LD ${locals.length} ${nextGlobal(label)}"), nextGlobal)
  }
}

case class DEBUG(i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (s, g1) = DEFVAR("debug", i).transpile(pos, locals, globals)
    val (v, g2) = VAR("debug").transpile(pos + s.length + 1, locals, g1)
    (s ++ Vector("DBUG") ++ v, g2)
  }
}

object Instruction {
  implicit def intToCONSTANT(i: Int) = CONSTANT(i)
  implicit def stringToVAR(s: String) = VAR(s)
}

object Program {
  def allocGlobalSpace(space: Int) =
    Vector(
      s"DUM $space",
      "LDC 0",
      "LDF 7",
      "AP 1",
      "LDF 18",
      s"RAP $space",
      "RTN",
      "LDC 0",
      "LD 0 0",
      s"LDC ${space - 1}",
      "CEQ",
      "TSEL 17 12",
      "LD 0 0",
      "LDC 1",
      "ADD",
      "LDF 7",
      "AP 1",
      "RTN")

  def apply(i: Instruction): String = {
    val preSteps = allocGlobalSpace(3)
    (preSteps ++ i.transpile(preSteps.length, List(), Map())._1 :+ "RTN\n").mkString("\n")
  }
}

object main extends App {
  // code goes here
  //Program(LET(("x", 3), ("y", 2))(ADD("x", "y")))
  Program(LET(
    ("sum", DEFUN("v1", "v2")(ADD("v1", "xpto"))),
    ("mul", DEFUN("v1", "v2")(MUL("v1", "v2"))))(FUNCALL("sum")(FUNCALL("mul")(2, 3), 4)))

  Program(LET(
    ("rec", DEFUN("v")(TFUNCALL("self")(ADD("v", 1)))))(FUNCALL("rec")("xpto")))

  Program(LET(
    ("rec", DEFUN("v")(TIF(EQ("v", 10), "v", TFUNCALL("self")(ADD("v", 1))))))(FUNCALL("rec")(1)))

  Program(LET(
    ("main", DEFUN("map")(ADD(2, "xpto"))))(CONS("main", 0)))

  Program(OR(EQ(1, 0), EQ(0, 0)))
  Program(AND(EQ(1, 0), EQ(0, 0)))

  Program(NOT(OR(EQ(1, 0), EQ(0, 0))))
  Program(NOT(AND(EQ(1, 0), EQ(0, 0))))

  Program(PROGN(DEBUG(DEFVAR("x", 3)), DEBUG(DEFVAR("y", 2)), ADD("x", "y")))

//  Program(LET(("x", DEFUN("a1", "a2")(ADD("a1", "a2"))), ("y", 2))(FUNCALL("x")(10, "y")))
  //LET(("body", n), FUNCALL("body", )ADD(ADD("4", 2), SUB(2, MUL(3, 5))))
  println("hello")
}
