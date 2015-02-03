/**
 * Created by Шолохов on 16.11.2014.
 */
case class Negation(expression : Expression) extends Expression{
  override def evaluate(values: Map[String, Boolean]): Boolean = !expression.evaluate(values)

  override def equals(o: Any): Boolean = {
    o match {
      case n : Negation => expression.equals(n.expression)
      case _ => false
    }
  }

  override def toString() : String =  "(" + "!" + expression.toString + ")"
}
