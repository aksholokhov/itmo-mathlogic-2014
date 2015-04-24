import scala.util.parsing.combinator.RegexParsers



class OrdinalParser extends RegexParsers {
  val number = "[0-9]+".r^^{x=>Num(x.toInt)}
  val w = "w"^^(_ => W())

  def expr: Parser[Ordinal] = (addable ~ opt("+" ~ expr)) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ b) => add(a, b)
  }

  def addable: Parser[Ordinal] =  (mullable ~ opt("*" ~ addable)) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ b) => mul(a, b)
  }

  def mullable: Parser[Ordinal] = (term ~ opt("^" ~ term)) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ b) => pow(a, b)
  }

  def term: Parser[Ordinal] = number | w | "(" ~ expr ~ ")" ^^ {case _ ~ e ~ _ => e}
}
