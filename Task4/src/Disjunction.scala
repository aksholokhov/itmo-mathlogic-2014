/**
 * Created by Шолохов on 16.11.2014.
 */
case class Disjunction (override val left : Expression, override val right : Expression) extends BinOp (left, right){
  override def apply(l: Boolean, r: Boolean): Boolean = l || r

  override def equals(o: Any): Boolean = {
    o match {
      case n : Disjunction => left.equals(n.left) && right.equals(n.right)
      case _ => false
    }
  }

  override def toString() : String = "(" + left.toString + "|" + right.toString + ")"

}
