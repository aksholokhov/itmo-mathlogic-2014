/**
 * Created by Шолохов on 30.01.2015.
 */
case class Exists(override val variable : Variable, override val expression : Expression) extends Quantifier (variable, expression){
  override def toString(): String = "?" + super.toString()
  override def equals(o : Any): Boolean = o.isInstanceOf[Exists] && super.equals(o)
}
