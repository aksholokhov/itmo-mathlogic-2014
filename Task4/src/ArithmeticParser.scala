import scala.util.parsing.combinator.RegexParsers

class ArithmeticParser extends RegexParsers {
  val numbers = "[0..9]+".r
  val charsL = "[a..z]".r
  val charsB = "[A..Z]".r
k
  def expression: Parser[Expression] = (disjunction ~ opt("->" ~ expression)) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ b) => Implication(a, b)
  }

  def disjunction: Parser[Disjunction] =  (conjunction ~ opt("|" ~ disjunction)) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ b) => Disjunction(a, b)
  }

  def conjunction: Parser[Conjunction] = (unary ~ opt("&" ~ conjunction)) ^^ {
    case a ~ None => a
    case a ~ Some(op ~ b) => Conjunction(a, b)
  }

  def unary: Parser[Expression] = predicate | (("!" ~ unary) ^^ {case op ~ a => Negation(a)}) | ("(" ~ expression ~ ")") ^^ {
    case "(" ~ a ~ ")" => a
  }


  def term: Parser[Ordinal] = number | w | "(" ~ expr ~ ")" ^^ {case _ ~ e ~ _ => e}
}
