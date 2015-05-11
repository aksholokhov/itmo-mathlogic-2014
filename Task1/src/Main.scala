

import java.io.PrintWriter

import scala.io.Source

/**
 * Created by alex on 16.11.14.
 */
object Main {

  var expressions: Array[(Expression, Int)] = null

  def mpVerifyTo(e : Expression, max : Int) : (Boolean, Int, Int) = {
    for (j <- max-1 to 0 by -1) {
      if (expressions(j)._1.isInstanceOf[Implication]) {
        val t = expressions(j)._1.asInstanceOf[Implication]
        if (t.right.equals(e)) {
          for (k <- max - 1 to 0 by -1) {
            if (expressions(k)._1.equals(t.left)) {
              return (true, k, j)
            }
          }
        }
      }
    }
    
    (false, 0, 0)
  }


  
  def main (args: Array[String]) {
    val strs = Source.fromFile("good6.in").getLines().toArray
    val out: PrintWriter = new PrintWriter("output.txt")

    expressions = strs.map(new MyParser(_).parse(0)).zip(1 to strs.length)

    for ((e, num) <- expressions ) {
      Axioms.checker(e) match {
        case -1 =>
          mpVerifyTo(e, num) match {
            case (true, a, b) => out.println("(" + num + ") " + strs(num-1) + " (M.P. " + (a+1) + " " + (b+1) + ")");
            case (false, _, _) =>
              out.println("(" + num + ") " + strs(num-1) + " (не доказано)")
              out.close()
              return
        }
        case _ => out.println("(" + num + ") " + strs(num-1) + " (сх. акс. " + Axioms.checker(e) + ")" );
      }

    }
    out.close()
  }
}
