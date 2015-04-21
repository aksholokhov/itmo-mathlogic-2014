
object Main {
  def main(args: Array[String]) {
    val str = "w=1"
    val parser = new OrdinalParser
    println(parser.parseAll(parser.term, str))
  }
}
