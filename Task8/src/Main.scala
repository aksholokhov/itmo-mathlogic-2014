
object Main {
 def main(args: Array[String]) {
    val str = "w=w^w".split("\\=")
    val parser = new OrdinalParser

    val ord1 = parser.parseAll(parser.expr, str(0)).get
    val ord2 = parser.parseAll(parser.expr, str(1)).get
    println(ord1 + " = " + ord2)
    val c = new Atom(0)
    println(c.ordToCantor(ord1) + " = " + c.ordToCantor(ord2))
  }

}
