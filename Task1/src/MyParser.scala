
class MyParser {

  var str = ""
  var length = 0

  def this (str : String) {
    this()
    this.str = str.replaceAll("->", ">")
    this.length = this.str.length
  }

  def parse (start : Int, end : Int = length) : Expression = {
    var balance = 0
  //  println(str.substring(start, end))
    for (i <- start until end) {
      val c = str(i)
      c match {
        case '(' => balance+=1
        case ')' => balance-=1
        case '>' if balance == 0 => return new Implication(parse(start, i), parse(i+1, end))
        case _ => ;
      }
    }

    balance = 0
    for (i <- start until end) {
      val c = str(i)
      c match {
        case '(' => balance+=1
        case ')' => balance-=1
        case '|' if balance == 0 => return new Disjunction(parse(start, i), parse(i+1, end))
        case _ => ;
      }
    }

    balance = 0
    for (i <- start until end) {
      val c = str(i)
      c match {
        case '(' => balance+=1
        case ')' => balance-=1
        case '&' if balance == 0 => return new Conjunction(parse(start, i), parse(i+1, end))
        case _ => ;
      }
    }

    if (str(start).equals('!')) {
      return new Negation(parse(start+1, end))
    }

    if (str(start).isLetterOrDigit) {
      var i = start
      var expr = ""
      while (i < end && str(i).isLetterOrDigit) {
  //      println("Var")
        expr += str(i)
        i+=1
      }
      return new Variable(expr)
    }

    if (str(start).equals('(')) {
      return parse(start +1, end-1)
    }

    return null
  }
}
