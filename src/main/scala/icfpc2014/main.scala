package icfpc2014

object Tests extends App {
  // code goes here
  //Program(LET(("x", 3), ("y", 2))(ADD("x", "y")))
  Program(LET(
    ("sum", DEFUN("v1", "v2")(ADD("v1", "v2"))),
    ("mul", DEFUN("v1", "v2")(MUL("v1", "v2"))))(FUNCALL("sum")(FUNCALL("mul")(2, 3), 4)))

  Program(LET(
    ("rec", DEFUN("v")(TFUNCALL("self")(ADD("v", 1)))))(FUNCALL("rec")(1)))

  Program(LET(
    ("rec", DEFUN("v")(TIF(EQ("v", 10), "v", TFUNCALL("self")(ADD("v", 1))))))(FUNCALL("rec")(1)))

  Program(PROGN(DEBUG(DEFVAR("x", 3)), DEBUG(DEFVAR("y", 2)), ADD("x", "y")))

  Program(PROGN(DEFVAR("fun", DEFUN("x")(MUL("x", "x"))), FUNCALL("fun")(2)))

  Program(PROGN(DEFVAR("f", (DEFUN()(ADD(2, 2)))), FUNCALL("f")()))

  val p = new Compiler(
    """(let ((rec (defun (v) (recur (+ v 1)))))
      |  (rec 1))
    """.stripMargin).compile

  val f = new Compiler(
    """(let ((x 2) (y (= 4 4)) (f (defun (x) (<= 1 x)))) (f (+ x y)))
    """.stripMargin).compile

  val q = new Compiler(
    """(let ((rec (defun (v) (self (+ v 1)))))
      |(rec 1))""".stripMargin).compile

  val dummy = new Compiler(
    """
      |(+
      |   (- 50 30)
      |   (* 10 10)
      |)""".stripMargin).compile

  val r = new Compiler(
    """(let ((rec (defun (v) (if (= v 10) v (self (+ v 1))))))
      |(rec 1))
      |""".stripMargin).compile

  val s = new Compiler(
    """
      |(let ((rec (defun (v) (tif (= v 10) v (recur (+ v 1))))))
      |      (rec 1))""".stripMargin).compile

  println()

  println(dummy)
  println(f)
  println(p)
  println(q)
  println(r)
  println(s)
}
