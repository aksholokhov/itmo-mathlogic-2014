/**
 * Created by Шолохов on 16.11.2014.
 */
case class Negation(e : Expression) extends Expression{
  override def evaluate(values: Map[String, Boolean]): Boolean = !e.evaluate(values)

  override def equals(o: Any): Boolean = {
    o match {
      case n : Negation => e.equals(n.e)
      case _ => false
    }
  }

  override def toString() : String =  "(" + "!" + e.toString + ")"
}
