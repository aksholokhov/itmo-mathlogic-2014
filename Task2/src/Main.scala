
import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
 * Created by alex on 16.11.14.
 */
object Main {

  val expressions, context, answer: ArrayBuffer[Expression] = new ArrayBuffer[Expression]()
  var alpha, beta: Expression = null

  def isMP(e : Expression, max : Int) : Boolean = {
    for (j <- 0 to max-1) {
      if (expressions(j).isInstanceOf[Implication]) {
        val t = expressions(j).asInstanceOf[Implication]
        if (t.right.equals(e)) {
          for (k <- 0 to max - 1) {
            if (expressions(k).equals(t.left)) {
              answer+=new Implication(new Implication(alpha, expressions(k)), new Implication(new Implication(alpha, new Implication(expressions(k), e)), new Implication(alpha, e)))
              answer+=new Implication(new Implication(alpha, new Implication(expressions(k), e)), new Implication(alpha, e))
              answer+=new Implication(alpha, e)
              return true
            }
          }
        }
      }
    }
    false
  }

  def isInContextOrAx(e: Expression) : Boolean = {
    if (Axioms.checker(e) != -1) return true
    else for (c <- context) {
      if (c.equals(e)) return true
    }
    false
  }

  def main (args: Array[String]) {
    val strs = Source.fromFile("input.txt").getLines().toArray
    val out: PrintWriter = new PrintWriter("output.txt")
    val contextStrings = strs(0).split(",")
    for (i <- 0 until contextStrings.length - 1) context += new MyParser(contextStrings(i)).parse(0)
    for (i <- 1 until strs.length - 1) expressions += new MyParser(strs(i)).parse(0)
    alpha = contextStrings(contextStrings.length-1).split("\\|-").map(new MyParser(_).parse(0)).take(1)(0)
    var correct = true
    var counter = 0

    for (e<-expressions if correct) {
      correct = false
      counter += 1
      if (!isInContextOrAx(e)) {
        if (alpha.equals(e)) {
          answer += new Implication(alpha, new Implication(alpha, alpha))
          answer += new Implication(new Implication(alpha, new Implication(alpha, alpha)), new Implication(new Implication(alpha, new Implication(new Implication(alpha, alpha), alpha)), new Implication(alpha, alpha)))
          answer += new Implication(new Implication(alpha, new Implication(new Implication(alpha, alpha), alpha)), new Implication(alpha, alpha))
          answer += new Implication(alpha, new Implication(new Implication(alpha, alpha), alpha))
          correct = true
        } else {
          correct = isMP(e, counter)
        }
      } else {
        answer += e
        answer += new Implication(e, new Implication(alpha, e))
        answer += new Implication(alpha, e)
        correct = true
      }
    }

    answer.foreach(e =>out.println(e))
    if (!correct) {
      out.println("Error")
    }

    out.close()
  }
}
