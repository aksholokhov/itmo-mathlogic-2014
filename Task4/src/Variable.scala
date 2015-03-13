/**
 * Created by Шолохов on 16.11.2014.
 */
case class Variable(name : String) extends Expression {
  def evaluate (values : Map[String, Boolean]) : Boolean = values(name)

  override def equals(o: Any): Boolean = {
    o match {
      case n : Variable => name.equals(n.name)
      case Plus(z : Zero, v: Variable) => name.equals(v.name)
      case Plus(v: Variable, z: Zero) => name.equals(v.name)
      case _ => false
    }
  }

  override def toString() : String =  name
}
