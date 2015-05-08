package parsing

import scala.util.parsing.combinator._
import scala.util.parsing.input.Position

object KarelSyntax extends JavaTokenParsers {

  def parse(input: String) = parseAll(program, input)

  def program = rep(commandDefinition) ^^ Program

  def commandDefinition = positioned(commandHeader ~ block ^^ {
    case name ~ body => Def(name, body)
  })

  def commandHeader = "void\\b".r ~> ident <~ emptyParens

  def block: Parser[Block] = "{" ~> rep(statement) ~ closingBrace ^^ {
    case statements ~ brace => Block(statements, brace)
  }

  def closingBrace = positioned("}" ^^^ ClosingBrace())

  def statement = positioned(
    ifStatement |
      whileStatement |
      repeatStatement |
      commandCall)

  def ifStatement: Parser[Statement] =
    ("if" ~> parenCond) ~ block ~ opt("else" ~> block) ^^ {
      case cond ~ th3n ~ None => IfThen(cond, th3n)
      case cond ~ th3n ~ Some(e1se) => IfThenElse(cond, th3n, e1se)
    }

  def whileStatement =
    ("while" ~> parenCond) ~ block ^^ { case cond ~ block => While(cond, block) }

  def repeatStatement =
    ("repeat" ~ "(" ~> integer <~ ")") ~ block ^^ { case times ~ block => Repeat(times.toInt, block) }

  def integer =
    "[1-9]\\d{0,2}".r

  def commandCall =
    ident <~ emptyParens ~ ";" ^^ Call

  def emptyParens = "(" ~ ")"

  def parenCond = "(" ~> condition <~ ")"

  def condition = disjunction

  def disjunction = binaryOperation(operator("||"), conjunction, Disjunction)

  def conjunction = binaryOperation(operator("&&"), primaryCondition, Conjunction)

  def binaryOperation(operator: Parser[Operator], operand: Parser[Condition], wrapper: (Condition, Operator, Condition) => Condition) =
    operand ~ rep(operator ~ operand) ^^ {
      case x ~ Nil => x
      case x ~ xs => xs.foldLeft(x)((acc, opcond) => wrapper(acc, opcond._1, opcond._2))
    }

  def operator(op: String): Parser[Operator] = positioned(literal(op) ^^ Operator)

  def primaryCondition: Parser[Condition] =
    positioned(booleanLiteral) |
      positioned(builtinPredicate <~ emptyParens) |
      operator("!") ~ primaryCondition ^^ { case a ~ b => Not(a, b) } |
      parenCond ^^ Parenthesized

  def booleanLiteral =
    "false" ^^^ False() |
      "true" ^^^ True()

  def builtinPredicate =
    "onBeeper" ^^^ OnBeeper() |
      "beeperAhead" ^^^ BeeperAhead() |
      "leftIsClear" ^^^ LeftIsClear() |
      "frontIsClear" ^^^ FrontIsClear() |
      "rightIsClear" ^^^ RightIsClear()
}
