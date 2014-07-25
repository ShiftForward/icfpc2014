package icfpc2014

sealed trait Instruction {
  type Definition = (String, Instruction)
  type Labels = List[Map[String, Int]]

  def transpile(pos: Int, labels: Labels): Vector[String]
}

trait BinaryOp extends Instruction {
  def i1: Instruction
  def i2: Instruction
  def op: String
  def transpile(pos: Int, labels: Labels) = {
    val s1 = i1.transpile(pos, labels)
    val s2 = i2.transpile(pos + s1.length, labels)
    s1 ++ s2 :+ op
  }
}

trait UnaryOp extends Instruction {
  def i: Instruction
  def op: String
  def transpile(pos: Int, labels: Labels) = {
    i.transpile(pos, labels) :+ op
  }
}

case class VAR(s: String) extends Instruction {
  def transpile(pos: Int, labels: Labels) = {
    labels.indexWhere(m =>  m.contains(s)) match {
      case -1 =>
        throw new Exception("Unknown variable")

      case i =>
        if (s == "self")
          Vector(s"LDF ${labels(i)(s)}")
        else
          Vector(s"LD $i ${labels(i)(s)}")
    }
  }

  override def toString = s
}

case class CONSTANT(v: Int) extends Instruction {
  def transpile(pos: Int, labels: Labels) = Vector(s"LDC $v")
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
  def transpile(pos: Int, labels: Labels) = {
    val (p, instructions, nextLabels, _) =
      definitions.foldLeft((pos, Vector[String](), Map[String, Int](), 0)) { case ((pos, instructions, nextLabels, i), (label, defin)) =>
        val s = defin.transpile(pos, labels)
        (pos + s.length, instructions ++ s, nextLabels + (label -> i), i + 1)
      }
    val newPos = p + 3
    instructions ++ Vector(s"LDF $newPos", s"AP ${definitions.length}") ++ Vector("RTN") ++ i.transpile(newPos, nextLabels :: labels)
  }

  override def toString = s"(let (${definitions.map { case (s, i) => s"($s $i)" }.mkString(" ")}) $i)"
}

case class DEFUN(args: String*)(i: Instruction) extends Instruction {
  def transpile(pos: Int, labels: Labels) = {
    val s = i.transpile(pos + 3, (args.zipWithIndex.toMap + ("self" -> (pos + 3))) :: labels) :+ "RTN"
    Vector(s"LDF ${pos + 3}", "LDC 1", s"TSEL ${pos + s.length + 3} 0") ++ s
  }

  override def toString = s"(defun (${args.mkString(" ")}) $i)"
}

case class FUNCALL(label: VAR)(parameters: (Instruction)*) extends Instruction {
  def transpile(pos: Int, labels: Labels) = {
    val (p, instructions) = parameters.foldLeft((pos, Vector[String]())) { case ((pos, instructions), parameter) =>
      val s = parameter.transpile(pos, labels)
      (pos + s.length, instructions ++ s)
    }
    instructions ++ label.transpile(p, labels) :+ s"AP ${parameters.length}"
  }

  override def toString = s"($label ${parameters.mkString(" ")})"
}

case class TFUNCALL(label: VAR)(parameters: (Instruction)*) extends Instruction {
  def transpile(pos: Int, labels: Labels) = {
    val (p, instructions) = parameters.foldLeft((pos, Vector[String]())) { case ((pos, instructions), parameter) =>
      val s = parameter.transpile(pos, labels)
      (pos + s.length, instructions ++ s)
    }
    instructions ++ label.transpile(p, labels) :+ s"TAP ${parameters.length}"
  }

  override def toString = s"(recur ${parameters.mkString(" ")})"
}

case class IF(pred: Instruction, thenInst: Instruction, elseInst: Instruction) extends Instruction {
  def transpile(pos: Int, labels: Labels) = {
    val ps = pred.transpile(pos, labels)
    val pi = pos + ps.length + 3
    val ts = thenInst.transpile(pi, labels) :+ "JOIN"
    val ti = pi + ts.length
    val es = elseInst.transpile(ti, labels) :+ "JOIN"
    val ei = ti + es.length
    ps ++ Vector(s"SEL $pi $ti", "LDC 1", s"TSEL $ei 0") ++ ts ++ es
  }

  override def toString = s"(if $pred $thenInst $elseInst)"
}

case class TIF(pred: Instruction, thenInst: Instruction, elseInst: Instruction) extends Instruction {
  def transpile(pos: Int, labels: Labels) = {
    val ps = pred.transpile(pos, labels)
    val pi = pos + ps.length + 1
    val ts = thenInst.transpile(pi, labels) :+ "RTN"
    val ti = pi + ts.length
    val es = elseInst.transpile(ti, labels) :+ "RTN"
    val ei = ti + es.length
    ps ++ Vector(s"TSEL $pi $ti") ++ ts ++ es
  }

  override def toString = s"(tif $pred $thenInst $elseInst)"
}

object Instruction {
  implicit def intToCONSTANT(i: Int) = CONSTANT(i)
  implicit def stringToVAR(s: String) = VAR(s)
}

object Program {
  def apply(i: Instruction): String =
    i.transpile(0, List()).mkString("\n") + "\nRTN\n"
}