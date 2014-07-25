package icfpc2014

import org.parboiled2._
import scala.util._

class Compiler(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Instruction] = rule {
    WhiteSpace ~ (Let | Defun | Call | Literal ) ~ WhiteSpace
  }

  def Call = rule { open ~ Opcode ~ close }

  def Opcode = rule { "+"     ~ Param2      ~> ADD |
                      "-"     ~ Param2      ~> SUB |
                      "*"     ~ Param2      ~> MUL |
                      "/"     ~ Param2      ~> DIV |
                      ">"     ~ Param2      ~> GT  |
                      ">="    ~ Param2      ~> GTE |
                      "="     ~ Param2      ~> EQ  |
                      "<="    ~ Param2      ~> LTE |
                      "<"     ~ Param2      ~> LT  |
                      "if"    ~ Param3      ~> IF  |
                      "tif"   ~ Param3      ~> TIF |
                      "recur" ~ ParamsArray ~> { (ps) => TFUNCALL("self")(ps: _*) } |
                      Text    ~ ParamsArray ~> { (f, ps) => FUNCALL(f)(ps: _*) } }

  /* Deal with 'let' */
  def Let     = rule { (open ~ "let" ~ LetDefs ~ Expression ~ close) ~> { (s, e) => LET(s : _*)(e) } }
  def LetDefs = rule { open ~ oneOrMore(Def).separatedBy(WhiteSpace) ~ close }
  def Def     = rule { (open ~ Text ~ Expression ~ close) ~> { (v, e) => (v, e) } }
  def open    = rule { WhiteSpace ~ "(" ~ WhiteSpace }
  def close   = rule { WhiteSpace ~ ")" ~ WhiteSpace }

  /* Deal with 'defun' */
  def Defun   = rule { open ~ "defun" ~ Lambdas ~ Expression ~ close ~> { (s, i) => DEFUN(s: _*)(i) } }
  def Lambdas = rule { open ~ oneOrMore(Text).separatedBy(WhiteSpace) ~ close }

  /* Parameters */
  def Param2      = rule { Expression ~ Expression }
  def Param3      = rule { Expression ~ Expression ~ Expression }
  def ParamsArray = rule { oneOrMore(Expression).separatedBy(WhiteSpace) }

  /* Lexic */
  def WhiteSpace  = rule { zeroOrMore(ch(' ') | ch('\t') | ch('\n')) }
  def Literal     = rule { Variable | Number }
  def Text        = rule { capture(oneOrMore(CharPredicate.Alpha)) }
  def Variable    = rule { Text ~> VAR }
  def Number      = rule { capture(Digits) ~> { j => CONSTANT(j.toInt) } }
  def Digits      = rule { optional("-") ~ oneOrMore(CharPredicate.Digit) }

  def compile: Instruction = InputLine.run() match {
    case Success(x: Instruction) => x
    case _                       => null
  }
}

