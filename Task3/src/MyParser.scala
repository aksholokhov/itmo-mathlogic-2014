import scala.collection.mutable.ArrayBuffer

class MyParser {

  var str = ""
  var length = 0
  var vars : ArrayBuffer[String] = null

  def this (str : String) {
    this()
    this.str = str.replaceAll("->", ">")
    this.length = this.str.length
    this.vars = new ArrayBuffer[String]()
  }

  def parse (start : Int, end : Int = length) : Expression = {
    var balance = 0
 //   println("New: (" + str.substring(start, end) + ")")
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
        expr += str(i)
        i+=1
      }
      if (!vars.contains(expr)) {
        vars += expr
      //  println(expr + " added")
      }

      return new Variable(expr)
    }

    if (str(start).equals('(')) {
      return parse(start +1, end-1)
    }

    return null
  }
}
