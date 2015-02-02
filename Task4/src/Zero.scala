/**
 * Created by Шолохов on 02.02.2015.
 */
case class Zero() extends Expression{
  override def evaluate(values: Map[String, Boolean]): Boolean = false
  override def toString(): String = "0"
  override def equals(o: Any): Boolean = {
    o match {
      case Zero => true
      case _ => false
    }
  }
}
