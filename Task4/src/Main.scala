/**
 * Created by Шолохов on 26.01.2015.
 */
import java.io.PrintWriter


import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Main {

  def getExchange(ex1: Expression, ex2: Expression): Expression = {
    (ex1, ex2) match {
      case (v: Variable, _) => if (ex2 != ex1) ex2 else null
      case (op1: BinOp, op2: BinOp) =>
        val t = getExchange(op1.left, op2.left)
        if (t != null) t else getExchange(op1.right, op2.right)
      case (q1: Quantifier, q2: Quantifier) =>
        val t = getExchange(q1.variable, q2.variable)
        if (t != null) t else getExchange(q1.expression, q2.expression)
      case (p1: Predicate, p2: Predicate) =>
        if (p1.terms.size == p2.terms.size) {
          p1.terms.zip(p2.terms).foreach(x => {
            val t = getExchange(x._1, x._2);
            if (t != null) return t
          })
        }
        null
      case (n1: Negation, n2: Negation) => getExchange(n1.e, n2.e)
      case _ => null
    }
  }

  def getVariables(e: Expression): ArrayBuffer[Variable] = {
    val ans = new ArrayBuffer[Variable]
    e match {
      case v : Variable => ans += v
      case op : BinOp =>
        ans ++= getVariables(op.left)
        ans ++= getVariables(op.right)
      case q : Quantifier =>
        ans += q.variable
        ans ++= getVariables(q.expression)
      case p : Predicate =>
        p.terms.foreach(ans++=getVariables(_))
      case n : Negation =>
        ans ++= getVariables(n.e)
    }
    ans
  }

  def getChainedVariables(e: Expression): ArrayBuffer[Variable] = {
    val ans = new ArrayBuffer[Variable]
    e match {
      case op : BinOp =>
        ans ++= getVariables(op.left)
        ans ++= getVariables(op.right)
      case q : Quantifier =>
        ans += q.variable
      case p : Predicate =>
        p.terms.foreach(ans++=getChainedVariables(_))
      case n : Negation =>
        ans ++= getChainedVariables(n.e)
    }
    ans
  }

  def getFreeVariables(e: Expression, chained : ArrayBuffer[Variable]): ArrayBuffer[Variable] = {
    val ans = new ArrayBuffer[Variable]
    e match {
      case v : Variable => if (!chained.contains(v)) ans += v
      case op : BinOp =>
        ans ++= getVariables(op.left)
        ans ++= getVariables(op.right)
      case q : Quantifier =>
        val isChained = chained.contains(q.variable)
        if (!isChained) {
          chained += q.variable
        }
        ans ++= getFreeVariables(q.expression, chained)
        if (!isChained) {
          chained -= q.variable
        }
      case p : Predicate =>
        p.terms.foreach(ans++=getFreeVariables(_, chained))
      case n : Negation =>
        ans ++= getFreeVariables(n.e, chained)
    }
    ans
  }

  def replace(e : Expression, a : Expression, b : Expression, c : Expression, v : Variable): Expression = {
    e match {
      case t: Variable => e
      case c: Conjunction => new Conjunction(replace(c.left, a, b, c, v), replace(c.right, a, b, c, v))
      case c: Disjunction => new Disjunction(replace(c.left, a, b, c, v), replace(c.right, a, b, c, v))
      case c: Implication => new Implication(replace(c.left, a, b, c, v), replace(c.right, a, b, c, v))
      case n: Negation => new Negation(replace(n.e, a, b, c, v))
      case c: Exists => new Exists(v, replace(c.expression, a, b, c, v))
      case c: ForAll => new ForAll(v, replace(c.expression, a, b, c, v))
      case p: Predicate =>
        p.name match {
          case "A" => a
          case "B" => b
          case "C" => c
          case _ => null
        }
      case _ => null
    }
  }


  def main (args: Array[String]) {
    val strs = Source.fromFile("true6.in").getLines().toArray
    val out: PrintWriter = new PrintWriter("output.txt")


  }
}
