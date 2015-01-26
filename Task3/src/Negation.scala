/**
 * Created by Шолохов on 16.11.2014.
 */
case class Negation(val e : Expression) extends Expression{
  override def evaluate(values: Map[String, Boolean]): Boolean = !e.evaluate(values)

  override def equals(o: Any): Boolean = {
    if (o.isInstanceOf[Negation]) {
      e.equals(o.asInstanceOf[Negation].e)
    } else false
  }

  override def toString() : String = {
    return "(" + "!" + e.toString + ")"
  }
}
