
object Main {
  def main(args: Array[String]) {
    val str = "w^(w*2+3)=2*(3^w+5^w)".split("\\=")
    val parser = new OrdinalParser
    println(parser.parseAll(parser.expr, str(0)))
    println(parser.parseAll(parser.expr, str(1)))
  }
}
