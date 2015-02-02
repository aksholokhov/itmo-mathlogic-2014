/**
 * Created by Шолохов on 30.01.2015.
 */
class ForAll(override val variable : Variable, override  val expression : Expression) extends Quantifier (variable, expression){
  override def toString(): String = "@" + super.toString()
  override def equals(o : Any): Boolean = o.isInstanceOf[ForAll] && super.equals(o)
}

