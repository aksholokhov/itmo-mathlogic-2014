/**
 * Created by Шолохов on 02.02.2015.
 */
case class Apostrophe (expression : Expression) extends Expression{
  override def evaluate(values: Map[String, Boolean]): Boolean = expression.evaluate(values)
  override def toString(): String = expression.toString() + "'"
  override def equals(o: Any): Boolean = {
    o match {
      case a : Apostrophe => a.expression.equals(expression)
      case _ => false
    }
  }
}
