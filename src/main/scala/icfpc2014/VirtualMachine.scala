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
            throw new Exception(s"Unknown variable '$s'")
          case Some(i) =>
            (Vector(s"LD ${locals.length} ${i}"), globals)
        }

      case i =>
        (Vector(s"LD $i ${locals(i)(s)}"), globals)
    }
  }

  override def toString = s
}

case class CONSTANT(v: Int) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) =
    (Vector(s"LDC $v"), globals)
  override def toString = v.toString
}

case class ADD(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "ADD"
  override def toString = s"(+ $i1 $i2)"
}

case class SUB(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "SUB"
  override def toString = s"(- $i1 $i2)"
}

case class MUL(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "MUL"
  override def toString = s"(* $i1 $i2)"
}

case class DIV(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "DIV"
  override def toString = s"(/ $i1 $i2)"
}

case class EQ(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "CEQ"
  override def toString = s"(= $i1 $i2)"
}

case class GT(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "CGT"
  override def toString = s"(> $i1 $i2)"
}

case class GTE(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "CGTE"
  override def toString = s"(>= $i1 $i2)"
}

case class LT(i2: Instruction, i1: Instruction) extends BinaryOp {
  val op = "CGT"
  override def toString = s"(< $i1 $i2)"
}

case class LTE(i2: Instruction, i1: Instruction) extends BinaryOp {
  val op = "CGTE"
  override def toString = s"(<= $i1 $i2)"
}

case class ATOM(i: Instruction) extends UnaryOp {
  val op = "ATOM"
  override def toString = s"(atom? $i)"
}

case class CONS(i1: Instruction, i2: Instruction) extends BinaryOp {
  val op = "CONS"
  override def toString = s"(cons $i1 $i2)"
}

case class CAR(i: Instruction) extends UnaryOp {
  val op = "CAR"
  override def toString = s"(car $i)"
}

case class CDR(i: Instruction) extends UnaryOp {
  val op = "CDR"
  override def toString = s"(cdr $i)"
}

case class LET(definitions: (String, Instruction)*)(i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (p, instructions, nextLocals, nextGlobals, _) =
      definitions.foldLeft((pos, Vector[String](), Map[String, Int](), globals, 0)) { case ((pos, instructions, nextLocals, nextGlobals, j), (label, defin)) =>
        val (s, g) = defin.transpile(pos, locals, nextGlobals)
        (pos + s.length, instructions ++ s, nextLocals + (label -> j), g, j + 1)
      }
    val newPos = p + 4
    val (s, g) = i.transpile(newPos, nextLocals :: locals, nextGlobals)
    (instructions ++ Vector(
      s"DUM ${definitions.length}",
      s"LDF $newPos",
      s"RAP ${definitions.length}",
      "RTN") ++ s, g)
  }

  override def toString = s"(let (${definitions.map { case (s, i) => s"($s $i)" }.mkString(" ")}) $i)"
}

case class DEFUN(args: String*)(i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (st, g) = i.transpile(pos + 3, (args.zipWithIndex.toMap.mapValues(_ + 1) + ("self" -> 0)) :: locals, globals)
    val s = st :+ "RTN"
    val pre = Vector(
      s"LDF ${pos + 3}",
      "LDC 1",
      s"TSEL ${pos + s.length + 3} 0")
    (pre ++ s, g)
  }

  override def toString = s"[${args.mkString(" ")}] $i"
}

case class FUNCALL(label: VAR)(parameters: (Instruction)*) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (preLabel, g1) = label.transpile(pos, locals, globals)
    val (p, instructions, nextGlobals) = parameters.foldLeft((pos + preLabel.length, Vector[String](), g1)) { case ((pos, instructions, globals), parameter) =>
      val (s, g) = parameter.transpile(pos, locals, globals)
      (pos + s.length, instructions ++ s, g)
    }
    val (s, g) = label.transpile(p, locals, nextGlobals)
    (preLabel ++ instructions ++ s :+ s"AP ${parameters.length + 1}", g)
  }

  override def toString = s"($label ${parameters.mkString(" ")})"
}

case class TFUNCALL(label: VAR)(parameters: (Instruction)*) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (preLabel, g1) = label.transpile(pos, locals, globals)
    val (p, instructions, nextGlobals) = parameters.foldLeft((pos + preLabel.length, Vector[String](), g1)) { case ((pos, instructions, globals), parameter) =>
      val (s, g) = parameter.transpile(pos, locals, globals)
      (pos + s.length, instructions ++ s, g)
    }
    val (s, g) = label.transpile(p, locals, nextGlobals)
    (preLabel ++ instructions ++ s :+ s"TAP ${parameters.length + 1}", g)
  }

  override def toString = s"(recur ${parameters.mkString(" ")})"
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

  override def toString = s"(if $pred $thenInst $elseInst)"
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

  override def toString = s"(tif $pred $thenInst $elseInst)"
}

case class OR(i1: Instruction, i2: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) =
    GT(ADD(i1, i2), CONSTANT(0)).transpile(pos, locals, globals)

  override def toString = s"(or $i1 $i2)"
}

case class AND(i1: Instruction, i2: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) =
    EQ(ADD(i1, i2), CONSTANT(2)).transpile(pos, locals, globals)

  override def toString = s"(and $i1 $i2)"
}

case class NOT(i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) =
    EQ(i, CONSTANT(0)).transpile(pos, locals, globals)

  override def toString = s"(not $i)"
}

case class PROGN(i: Instruction*) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (_, instructions, nextGlobals) = i.foldLeft((pos + 1, Vector[String]("LDC 0"), globals)) { case ((pos, instructions, globals), instruction) =>
      val (st, g) = instruction.transpile(pos, locals, globals)
      val s = st ++ Vector("CONS", "CDR")
      (pos + s.length, instructions ++ s, g)
    }
    (instructions, nextGlobals)
  }

  override def toString = s"(progn ${i.mkString(" ")})"
}

case class DEFVAR(label: String, i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (s, g) = i.transpile(pos, locals, globals)
    val nextGlobal = g + (label -> g.getOrElse(label, g.size))
    (s ++ Vector(s"ST ${locals.length} ${nextGlobal(label)}", s"LD ${locals.length} ${nextGlobal(label)}"), nextGlobal)
  }

  override def toString = s"(defvar $label $i)"
}

case class DEBUG(i: Instruction) extends Instruction {
  def transpile(pos: Int, locals: Locals, globals: Globals) = {
    val (s, g1) = DEFVAR("debug", i).transpile(pos, locals, globals)
    val (v, g2) = VAR("debug").transpile(pos + s.length + 1, locals, g1)
    (s ++ Vector("DBUG") ++ v, g2)
  }

  override def toString = s"(debug $i)"
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
    val preSteps = allocGlobalSpace(50)
    (preSteps ++ i.transpile(preSteps.length, List(), Map())._1 :+ "RTN\n").mkString("\n")
  }
}
