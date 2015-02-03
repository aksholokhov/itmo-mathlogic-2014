/**
 * Created by Шолохов on 26.01.2015.
 */
import java.io.PrintWriter


import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Main {

  var proof: Array[Expression] = null
  var context: Array[Expression] = null
  var alpha: Expression = null
  var beta: Expression = null

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
            val t = getExchange(x._1, x._2)
            if (t != null) return t
          })
        }
        null
      case (n1: Negation, n2: Negation) => getExchange(n1.expression, n2.expression)
      case (a1: Apostrophe, a2: Apostrophe) => getExchange(a1.expression, a2.expression)
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
        ans ++= getVariables(n.expression)
      case a : Apostrophe =>
        getVariables(a.expression)
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
        ans ++= getChainedVariables(n.expression)
      case a : Apostrophe =>
        getChainedVariables(a.expression)
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
        ans ++= getFreeVariables(n.expression, chained)
      case a : Apostrophe =>
        ans ++= getFreeVariables(a.expression, chained)
    }
    ans
  }

  def replace(e : Expression, a : Expression, b : Expression, c : Expression, v : Variable): Expression = {
    e match {
      case t: Variable => e
      case c: Conjunction => new Conjunction(replace(c.left, a, b, c, v), replace(c.right, a, b, c, v))
      case c: Disjunction => new Disjunction(replace(c.left, a, b, c, v), replace(c.right, a, b, c, v))
      case c: Implication => new Implication(replace(c.left, a, b, c, v), replace(c.right, a, b, c, v))
      case n: Negation => new Negation(replace(n.expression, a, b, c, v))
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

  def isAxiomOrInContext(expr : Expression): Int = {
    if (context.contains(expr)) 1000
    else Axioms.checker(expr)
  }


  def main (args: Array[String]) {
    val strs = Source.fromFile("true6.in").getLines().toArray
    val out: PrintWriter = new PrintWriter("output.txt")

    val con = strs(0).split("\\|-")
    beta = new MyParser(con.last).parse(0)
    if (con.size > 2) {
      alpha = new MyParser(con(con.size-2)).parse(0)
      context = con.dropRight(2).map(new MyParser(_).parse(0))
    } else {
      context = con.dropRight(1).map(new MyParser(_).parse(0))
    }
    proof = con.drop(1).map(new MyParser(_).parse(0))

    var correct: Boolean = true
    var stopNumber = 0

    try {
      for ((num, expr) <- (1 to proof.length).zip(proof) if correct) {
        correct = false
        val c = isAxiomOrInContext(expr)
        (c, expr) match {
          case (11, Implication(forall: ForAll, right)) =>
            val term = getExchange(forall.expression, right)
            val v1 = getChainedVariables(forall)
            val v2 = getVariables(term)
            if (term != null && !forall.variable.equals(term)) {
              v2.foreach(x =>
                if (v1.contains(x)) throw new Exception(
                  "Вывод некорректен начиная с формулы " +
                    num + ": " + "терм " + term +
                    "не свободен для подстановки в формулу " +
                    forall.expression + " вместо переменной " +
                    forall.variable
                ))
            }

          case (12, Implication(left, ex: Exists)) =>
            val term = getExchange(ex.expression, left)
            val v1 = getChainedVariables(ex) += ex.variable
            val v2 = getVariables(term)
            if (term != null && !ex.variable.equals(term)) {
              v2.foreach(x =>
                if (v1.contains(x)) throw new Exception(
                  "Вывод некорректен начиная с формулы " +
                    num + ": " + "терм " + term +
                    "не свободен для подстановки в формулу "
                    + ex.expression + " вместо переменной " + ex.variable
                ))
            }

          case (13, Implication(Conjunction(_, fa: ForAll)), right) =>
            if (getChainedVariables(right).contains(fa.variable))
              throw new Exception("Вывод некорректен начиная с формулы " +
                num + ": " + "терм " + new Apostrophe(fa.variable) +
                "не свободен для подстановки в формулу " +
                right + " вместо переменной " + fa.variable)

          case (x, _) if x != -1 => correct = true

          case _ =>
            for (i <- (num - 1) to 0 by -1) {
              proof(i) match {
                case Implication(left, right) =>
                  if (right == expr)
                    for (j <- (num - 1) to 0 by -1) {
                      if (proof(j) == left) {
                        correct = true
                      }
                    }
              }
            }
            if (!correct) {
              expr match {
                case (Implication(left, right: ForAll)) =>
                  for (i <- (num - 1) to 0 by -1) {
                    proof(i) match {
                      case Implication(left2, right2) =>
                        if ((left == left2) && (right.expression == right2)) {
                          if (getFreeVariables(left, new ArrayBuffer[Variable]()).contains(right.variable)) {
                            throw new Exception("Вывод некорректен начиная с формулы " +
                              num + ": " + "переменная " + right.variable +
                              " входит свободно в формулу " + left)
                          }
                          correct = true
                        }
                    }
                  }
                case (Implication(left: Exists, right)) =>
                  for (i <- (num - 1) to 0 by -1) {
                    proof(i) match {
                      case Implication(left2, right2) =>
                        if ((right == right2) && (left.expression == left2)) {
                          if (getVariables(right).contains(left.variable) && !getChainedVariables(right).contains(left.variable)) {
                            throw new Exception("Вывод некорректен начиная с формулы " +
                              (i + 1) + ": " + "переменная " + left.variable +
                              " входит свободно в формулу " + right)
                          }
                          correct = true
                        }
                    }
                  }
                case _ => stopNumber = num
              }
            }

        }
      }
      if (correct) {
        out.println("Доказательство корректно")
      } else {
        out.println("Вывод некорректен начиная с формулы " + stopNumber)
      }
    }
    catch  {
      case e : Exception => out.println(e.getMessage)
    }
    if (correct) {

    }
  }
}
