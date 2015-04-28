import scala.util.parsing.combinator.RegexParsers

class ArithmeticParser extends RegexParsers {
  val nums = "[0..9]+".r
  val charsL = "[a..z]".r
  val charsB = "[A..Z]".r

  def expression: Parser[Expression] = addable | (expr ~ "+" ~ addable) ^^ {
    case a ~ "+" ~ b => add(a, b)
  }

  def addable: Parser[Ordinal] = mullable | (addable ~ "*" ~ mullable) ^^ {
    case a ~ "*" ~ b => mul(a, b)
  }

  def mullable: Parser[Ordinal] = term | (mullable ~ "^" ~ term) ^^ {
    case a ~ "^" ~ b => pow(a, b)
  }

  def term: Parser[Ordinal] = nums ^^(x => Num(x.toInt)) |
    "w" ^^ {_ => W()} |
    "(" ~ expr ~ ")" ^^ {case _ ~ e ~ _ => e}
}
