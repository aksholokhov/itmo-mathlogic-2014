

import java.io.PrintWriter

import scala.io.Source

/**
 * Created by alex on 16.11.14.
 */
object Main {
  def main (args: Array[String]) {
    val strs = Source.fromFile("maxtest1.in").getLines().toArray;
    val expr = strs.map(new MyParser(_).parse(0))
    val out: PrintWriter = new PrintWriter("output.txt")

    var correct = true
    var counter = 0

    for (e <- expr if correct) {
      correct = false
      counter += 1
      val axiom = Axioms.checker(e);

      if (axiom != -1) {
        correct = true
        out.println("(" + (counter) + ") " + strs(counter-1) + " (сх. акс. " + axiom + ")" )
      }
      if (!correct) {
        for (j <- 0 to counter-1 if !correct) {
          if (expr(j).isInstanceOf[Implication]) {
          val t = expr(j).asInstanceOf[Implication]
            if (t.right.equals(e)) {
              for (k <- 0 to counter - 1 if !correct) {
                if (expr(k).equals(t.left)) {
                  correct = true
                  out.println("(" + (counter) + ") " + strs(counter-1) + " (M.P. " + (k+1) + " " + (j+1) + ")")
                }
              }
            }
          }
        }
        if (!correct) {
          out.println("(" + (counter) + ") " + strs(counter-1) + " (не доказано)")
        }
      }
    }
    out.close()
  }
}
