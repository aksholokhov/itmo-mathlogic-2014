

import scala.collection.mutable.ArrayBuffer

/**
 * Created by alex on 16.11.14.
 */
class Deduction(val alpha: Expression, val beta: Expression, val context: ArrayBuffer[Expression], val expressions: ArrayBuffer[Expression]) {

  val answer: ArrayBuffer[Expression] = new ArrayBuffer[Expression]()

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

  def getProof() : Option[ArrayBuffer[Expression]] = {
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
          answer += new Implication(alpha, alpha)
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

    if (!correct) {
       Option.empty
    }
    else  Option.apply(answer)
  }
}
