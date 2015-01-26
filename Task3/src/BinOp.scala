/**
 * Created by Шолохов on 16.11.2014.
 */
abstract class BinOp(left : Expression, right : Expression) extends Expression {
  def evaluate (values : Map[String, Boolean]) : Boolean =
    apply(left.evaluate(values), right.evaluate(values))

  def apply(l : Boolean, r : Boolean) : Boolean
}
