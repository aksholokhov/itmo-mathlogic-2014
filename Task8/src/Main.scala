import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    tester("tests.txt")
  }

  def tester(file: String) = {
    val parser = new OrdinalParser
    val c = new Atom(0)
    val tests = Source.fromFile(file).getLines().map(s => s.split("\\s+"))        //Split a, compare symbol and b by spaces
      .map(s => (parser.parse(parser.expr, s(0)).get, s(1), parser.parse(parser.expr, s(2)).get)) //Transform a, b to otdinals
      .map(s => test(s._1, s._2, s._3)).foreach(println(_))
  }

  def test(a: Ordinal, true_ans: String, b: Ordinal): String = {
    val ords: String = a + " " + true_ans + " " + b
    val z = Atom(0)
    try {
      val cantorA = z.ordToCantor(a)
      val cantorB = z.ordToCantor(b)
      var myAns: String = ""
      cantorA.compare(cantorB) match {
        case n if n > 0 => myAns = ">"
        case n if n < 0 => myAns = "<"
        case 0 => myAns = "="
      }
      if (myAns.equals(true_ans)) {
        "YES: " + ords
      }
      else {
        "NO:  " + ords + ", found: " + myAns + ";"
      }
    } catch {
      case n: Exception => "ERR: " + ords + ", found: "  + n
     // case n: Error => "ERR: " + ords + ", found: "  + n
    }
  }

}
