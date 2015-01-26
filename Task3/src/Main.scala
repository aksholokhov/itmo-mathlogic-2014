/**
 * Created by Шолохов on 26.01.2015.
 */
import java.io.PrintWriter


import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Main {

  var expr : Expression = null
  var templates : ArrayBuffer[Array[Expression]] = null
  var vars : ArrayBuffer[String] = null
  var tree : ArrayBuffer[(Map[String, Boolean], ArrayBuffer[Expression])]  = null
  var answer : ArrayBuffer[Expression] = null

  def tautology() : (Boolean, Map[String, Boolean]) = {
    (0 to vars.length).map(i => (1 to i).map(x => true).padTo(vars.length, false).permutations.foreach(p => if (!expr.evaluate(vars.zip(p).toMap)) {return (false, vars.zip(p).toMap)}))
    (true, null)
  }

  def proof (map : Map[String, Boolean], depth : Int) {
    if (depth == vars.size) {
      answer = new ArrayBuffer[Expression]()
      proofGen(map, expr)
      val proof : (Map[String, Boolean], ArrayBuffer[Expression]) = (map, answer)
      tree += proof
      return
    }
    proof(map.+((vars(depth), true)), depth +1)
    proof(map.+((vars(depth), false)), depth +1)
  }

  def proofGen(map: Map[String, Boolean], expr: Expression): Boolean = {
    expr match {
      case Variable(name) => expr.evaluate(map)
      case Implication(left, right) => 
        val fl1: Boolean = proofGen(map, left)
        val fl2: Boolean = proofGen(map, right)
        if (fl1 && fl2) {
          templates(4).foreach(answer += replace(_, left, right))
          true
        }
        else if (fl1) {
          templates(5).foreach(answer += replace(_, left, right))
          false
        }
        else if (fl2) {
          templates(6).foreach(answer += replace(_, left, right))
          true
        }
        else {
          templates(7).foreach(answer += replace(_, left, right))
          true
        }
      
      case Conjunction(left, right) => 
        val fl1: Boolean = proofGen(map, left)
        val fl2: Boolean = proofGen(map, right)
        if (fl1 && fl2) {
          templates(0).foreach(answer += replace(_, left, right))
          true
        }
        else if (fl1) {
          templates(1).foreach(answer += replace(_, left, right))
          false
        }
        else if (fl2) {
          templates(2).foreach(answer += replace(_, left, right))
          false
        }
        else {
          templates(3).foreach(answer += replace(_, left, right))
          false
        }
      case Disjunction(left, right) =>
        val fl1: Boolean = proofGen(map, left)
        val fl2: Boolean = proofGen(map, right)
        if (fl1 && fl2) {
          templates(8).foreach(answer += replace(_, left, right))
          true
        }
        else if (fl1) {
          templates(9).foreach(answer += replace(_, left, right))
          true
        }
        else if (fl2) {
          templates(10).foreach(answer += replace(_, left, right))
          true
        }
        else {
          templates(11).foreach(answer += replace(_, left, right))
          false
        }
      
      case Negation(e) => 
        val fl: Boolean = proofGen(map, e)
        if (fl) {
          templates(12).foreach(answer += replace(_, e, null))
          false
        }
        else {
          templates(13).foreach(answer += replace(_, e, null))
          if (e.isInstanceOf[Negation]) {
            templates(14).foreach(answer += replace(_, e, null))
          }
          true
        }
      
      case _ => false
    }
  }

  def replace(ex: Expression, a: Expression, b: Expression): Expression = {
    ex match {
      case Implication(left, right) => new Implication(replace(left, a, b), replace(right, a, b))
      case Conjunction(left, right) => new Conjunction(replace(left, a, b), replace(right, a, b))
      case Disjunction(left, right) => new Disjunction(replace(left, a, b), replace(right, a, b))
      case Negation(e) => new Negation(replace(e, a, b))
      case Variable(name) => if (name == "A") a else b
      case _ => null
    }
  }

  def ans(): ArrayBuffer[Expression] = {
    while (tree.size != 1) {
      val p1: (Map[String, Boolean], ArrayBuffer[Expression]) = tree(0)
      val p2: (Map[String, Boolean], ArrayBuffer[Expression]) = tree(1)
      val alpha: Expression = new Variable(vars(p1._1.size - 1))
      var map: Map[String, Boolean] = p1._1
      map -= alpha.toString
      val g = new ArrayBuffer[Expression]
      for (v <- vars) {
        if (map.contains(v)) {
          if (map(v)) g+=new Variable(v)
          else g+=new Negation(new Variable(v))
        }
      }
      val deduction: Deduction = new Deduction(alpha, expr, g, p1._2)
      var proof = new ArrayBuffer[Expression]

      val t = deduction.getProof()
      if (t.isDefined) {
        proof = t.get
      } else  {
        return p1._2
      }

      proof ++= new Deduction(new Negation(alpha), expr, g, p2._2).getProof().get
      proof ++= ExcludedMiddle.getProof(alpha)
      proof += new Implication(new Implication(alpha, expr), new Implication(new Implication(new Negation(alpha), expr), new Implication(new Disjunction(alpha, new Negation(alpha)), expr)))
      proof += new Implication(new Implication(new Negation(alpha), expr), new Implication(new Disjunction(alpha, new Negation(alpha)), expr))
      proof += new Implication(new Disjunction(alpha, new Negation(alpha)), expr)
      proof += expr
      val s = (map, proof)
      tree += s
      tree.remove(0)
      tree.remove(0)
    }
    tree(0)._2
  }

  def main (args: Array[String]) {
    val strs = Source.fromFile("input.txt").getLines().toArray
    val out: PrintWriter = new PrintWriter("output.txt")

    val parser = new MyParser(strs(0))
    expr = parser.parse(0)
    vars = parser.vars
    templates = new ArrayBuffer[Array[Expression]]()
    tree = new ArrayBuffer[(Map[String, Boolean], ArrayBuffer[Expression])]()
    answer = new ArrayBuffer[Expression]

    for (i <- 1 to 15) {
      templates += Source.fromFile("expression" + i + ".txt").getLines().map(new MyParser(_).parse(0)).toArray
    }

    if(tautology()._1) {
      proof(Map.empty, 0)
      ans().foreach(out.println(_))
    }
    else {
      out.print("Высказывание ложно при ")
      tautology()._2.foreach(p => out.print(p._1 + "=" + (if (p._2)   "И, " else  "Л, ")))
    }
    out.close()

  }
}
