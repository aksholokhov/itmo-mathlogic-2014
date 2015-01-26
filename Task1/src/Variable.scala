/**
 * Created by Шолохов on 16.11.2014.
 */
case class Variable(val name : String) extends Expression {
  def evaluate (values : Map[String, Boolean]) : Boolean = values(name)

  override def equals(o: Any): Boolean = {
    if (o.isInstanceOf[Variable]) {
      (o.asInstanceOf[Variable]).name.equals(name)
    } else false
  }
}
