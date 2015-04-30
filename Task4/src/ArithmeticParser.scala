import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.RegexParsers

class ArithmeticParser extends RegexParsers {
  val numbers = "[0..9]+".r
  val charsL = "[a..z]".r
  val charsB = "[A..Z]".r
  val fa = "@".r
  val ex = "?".r

  def expression: Parser[Expression] = (disjunction ~ opt("->" ~ expression)) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ b) => Implication(a, b)
  }

  def disjunction: Parser[Expression] =  (opt(disjunction ~ "|") ~ conjunction) ^^ {
    case None ~ b => b
    case Some(a ~ op) ~ b => Disjunction(a, b)
  }

  def conjunction: Parser[Expression] = (opt(conjunction ~ "&") ~ unary) ^^ {
    case None ~ a  => a
    case Some(a ~ op) ~ b => Conjunction(a, b)
  }

  def unary: Parser[Expression] = predicate | (("!" ~ unary) ^^ {case op ~ a => Negation(a)}) | ("(" ~ expression ~ ")") ^^ {
    case "(" ~ a ~ ")" => a
  } | ((fa | ex) ~ variable ~ unary) ^^ {
    case "@" ~ x ~ f => ForAll(x, f)
    case "?" ~ x ~ f => Exists(x, f)
  }

  def variable: Parser[Variable] = charsL ~ numbers ^^ {case a => Variable(a.toString())}

  def predicate: Parser[Expression] = ((charsB ~ numbers) ~ ("(" ~> rep1sep(term, ",") <~ ")")) ^^ {
    case name ~ number ~ a =>  {
      val args = ArrayBuffer[Expression]() ++= a
      Predicate(name + number, args)
    }
  }

  def term: Parser[Expression] = (opt(term ~ "+") ~ addable) ^^ {
    case None ~ a  => a
    case Some(a ~ op) ~ b => Plus(a, b)
  }

  def addable: Parser[Expression] = (opt(addable ~ "*") ~ mullable) ^^ {
    case None ~ a  => a
    case Some(a ~ op) ~ b => Times(a, b)
  }

  def mullable: Parser[Expression] = (charsL ~ numbers ~ ("(" ~> rep1sep(term, ",") <~ ")" )) ^^ {
    case name ~ number ~ a => {
      val args = ArrayBuffer[Expression]() ++= a
      Function(name + number, args)
    }
  } | variable | "(" ~> term <~ ")" | "0"^^{_ => Zero()} | (mullable <~ "'")^^{a => Apostrophe(a)}

}
