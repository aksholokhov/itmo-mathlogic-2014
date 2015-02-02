/**
 * Created by Шолохов on 02.02.2015.
 */
case class Apostrophe (expr : Expression) extends Expression{
  override def evaluate(values: Map[String, Boolean]): Boolean = expr.evaluate(values)
  override def toString(): String = expr.toString() + "'"
  override def equals(o: Any): Boolean = {
    o match {
      case a : Apostrophe => a.expr.equals(expr)
      case _ => false
    }
  }
}
