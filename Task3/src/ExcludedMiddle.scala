import scala.collection.mutable.ArrayBuffer

/**
 * Created by Шолохов on 26.01.2015.
 */
object ExcludedMiddle {
  def contrPositionRule(alpha: Expression, beta: Expression): ArrayBuffer[Expression] = {
    var proof: ArrayBuffer[Expression] = new ArrayBuffer[Expression]
    val g: ArrayBuffer[Expression] = new ArrayBuffer[Expression]
    g += new Implication(alpha, beta)
    proof += new Implication(new Implication(alpha, beta), new Implication(new Implication(alpha, new Negation(beta)), new Negation(alpha)))
    proof += g(0)
    proof += new Implication(new Implication(alpha, new Negation(beta)), new Negation(alpha))
    proof += new Implication(new Negation(beta), new Implication(alpha, new Negation(beta)))
    proof += new Negation(beta)
    proof += new Implication(alpha, new Negation(beta))
    proof += new Negation(alpha)
    proof = new Deduction(new Negation(beta), new Negation(alpha), g, proof).getProof().get
    proof = new Deduction(g(0), new Implication(new Negation(beta), new Negation(alpha)), new ArrayBuffer[Expression], proof).getProof().get
    proof
  }

  def getProof(alpha: Expression): ArrayBuffer[Expression] = {
    val proof: ArrayBuffer[Expression] = new ArrayBuffer[Expression]
    proof += new Implication(alpha, new Disjunction(alpha, new Negation(alpha)))
    proof ++= contrPositionRule(alpha, new Disjunction(alpha, new Negation(alpha)))
    proof += new Implication(new Negation(new Disjunction(alpha, new Negation(alpha))), new Negation(alpha))
    proof += new Implication(new Negation(alpha), new Disjunction(alpha, new Negation(alpha)))
    proof ++= contrPositionRule(new Negation(alpha), new Disjunction(alpha, new Negation(alpha)))
    proof += new Implication(new Negation(new Disjunction(alpha, new Negation(alpha))), new Negation(new Negation(alpha)))
    proof += new Implication(new Implication(new Negation(new Disjunction(alpha, new Negation(alpha))), new Negation(alpha)), new Implication(new Implication(new Negation(new Disjunction(alpha, new Negation(alpha))), new Negation(new Negation(alpha))), new Negation(new Negation(new Disjunction(alpha, new Negation(alpha))))))
    proof += new Implication(new Implication(new Negation(new Disjunction(alpha, new Negation(alpha))), new Negation(new Negation(alpha))), new Negation(new Negation(new Disjunction(alpha, new Negation(alpha)))))
    proof += new Negation(new Negation(new Disjunction(alpha, new Negation(alpha))))
    proof += new Implication(new Negation(new Negation(new Disjunction(alpha, new Negation(alpha)))), new Disjunction(alpha, new Negation(alpha)))
    proof += new Disjunction(alpha, new Negation(alpha))
    proof
  }
}
