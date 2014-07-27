package icfpc2014

import org.parboiled2._
import scala.util._

class Compiler(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Instruction] = rule {
    WhiteSpace ~ (SExpr | OExpr | Parens) ~ WhiteSpace
  }

  def OExpr  = rule { Defun2 | Literal }
  def SExpr  = rule { open ~ (Let | Progn | Defun | Defvar | Defvar2 | Call) ~ close }
  def Parens = rule { open ~ Expression ~ close }

  def Call = rule {
    "atom?"  ~ Expression ~> ATOM  |
    "car"    ~ Expression ~> CAR   |
    "caar"   ~ Expression ~> { l => CAR(CAR(l)) } |
    "cadr"   ~ Expression ~> { l => CAR(CDR(l)) } |
    "cdr"    ~ Expression ~> CDR   |
    "cddr"   ~ Expression ~> { l => CDR(CDR(l)) } |
    "cdar"   ~ Expression ~> { l => CDR(CAR(l)) } |
    "debug"  ~ Expression ~> DEBUG |
    "not"    ~ Expression ~> NOT   |
    ">"      ~ Param2     ~> GT    |
    ">="     ~ Param2     ~> GTE   |
    "<="     ~ Param2     ~> LTE   |
    "<"      ~ Param2     ~> LT    |
    "cons"   ~ Param2     ~> CONS  |
    "if"     ~ Param3     ~> IF    |
    "tif"    ~ Param3     ~> TIF   |
    "+"      ~ ParamN     ~> { _.reduceLeft { ADD } }   |
    "-"      ~ ParamN     ~> { _.reduceLeft { SUB } }   |
    "*"      ~ ParamN     ~> { _.reduceLeft { MUL } }   |
    "/"      ~ ParamN     ~> { _.reduceLeft { DIV } }   |
    "or"     ~ ParamN     ~> { _.reduceLeft { OR  } }   |
    "and"    ~ ParamN     ~> { _.reduceLeft { AND } }   |
    "="      ~ ParamN     ~> { _.reduceLeft { EQ  } }   |
    "recur"  ~ ParamN     ~> { TFUNCALL("self")(_: _*) } |
    "list"   ~ ParamN     ~> { _.foldRight(CONSTANT(-2147483648): Instruction) { CONS } } |
    Text     ~ ParamN     ~> { FUNCALL(_)(_: _*) } }

  /* Deal with 'let' */
  def Let     = rule { "let" ~ LetDefs ~ Expression ~> { LET(_: _*)(_) } }
  def LetDefs = rule { open ~ oneOrMore(Def).separatedBy(WhiteSpace) ~ close }
  def Def     = rule { open ~ Text ~ Expression ~ close ~> { (_, _) } }
  def open    = rule { WhiteSpace ~ "(" ~ WhiteSpace }
  def close   = rule { WhiteSpace ~ ")" ~ WhiteSpace }
  def openS   = rule { WhiteSpace ~ "[" ~ WhiteSpace }
  def closeS  = rule { WhiteSpace ~ "]" ~ WhiteSpace }

  /* Deal with 'defun' */
  def Defun    = rule { ("defun" | "λ") ~ Lambdas ~ Expression ~> { DEFUN(_: _*)(_) } }
  def Defun2   = rule { Lambdas2 ~ Expression ~> { DEFUN(_: _*)(_) } }
  def Defvar   = rule { ("defvar" | "setvar") ~ WhiteSpace ~ Text ~ Expression ~> DEFVAR }
  def Defvar2  = rule { Text ~ WhiteSpace ~ ":" ~ Expression ~> DEFVAR }
  def Progn    = rule { "progn" ~ ParamN ~> { PROGN(_: _*) } }
  def Lambdas  = rule { open ~ TextN ~ close }
  def Lambdas2 = rule { openS ~ TextN ~ closeS }

  /* Parameters */
  def Param2  = rule { Expression ~ Expression }
  def Param3  = rule { Expression ~ Expression ~ Expression }
  def ParamN  = rule { zeroOrMore(Expression).separatedBy(WhiteSpace) }

  /* Lexic */
  def WhiteSpace = rule { zeroOrMore(ch(' ') | ch('\t') | ch('\n')) }
  def Literal    = rule { Variable | Number }
  def Text       = rule { capture(CharPredicate.Alpha ~ zeroOrMore(CharPredicate.Alpha | ch('?') | ch('-'))) }
  def TextN      = rule { zeroOrMore(Text).separatedBy(WhiteSpace) }
  def Variable   = rule { Text ~> VAR }
  def Number     = rule { capture(Digits) ~> { d => CONSTANT(d.toInt) } }
  def Digits     = rule { optional("-") ~ oneOrMore(CharPredicate.Digit) }

  def compile: Instruction = InputLine.run() match {
    case Success(x: Instruction) => x
    case _                       => null
  }
}
