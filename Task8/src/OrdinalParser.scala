import scala.util.parsing.combinator.RegexParsers



class OrdinalParser extends RegexParsers {
  val nums = "[0..9]+".r

  def equation: Parser[equ] = expr ~ "=" ~ expr ^^ {
    case a ~ "=" ~ b => equ(a, b);
  }

  def expr: Parser[Ordinal] = addable | (expr ~ "+" ~ addable) ^^ {
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
