/**
 * Created by Шолохов on 30.01.2015.
 */
abstract class Quantifier (val variable : Variable, val expression : Expression) extends Expression{
  override def evaluate(values: Map[String, Boolean]): Boolean = true
  override def toString(): String = variable + expression.toString
  override def equals(o : Any) : Boolean = if (o.isInstanceOf[Quantifier])
    variable.equals(o.asInstanceOf[Quantifier].variable) && expression.equals(o.asInstanceOf[Quantifier].expression) else false
}
