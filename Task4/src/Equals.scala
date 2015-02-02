/**
 * Created by Шолохов on 02.02.2015.
 */
case class Equals (override val left: Expression, override val right: Expression) extends BinOp(left, right){
  override def apply(l : Boolean, r : Boolean) : Boolean = l == r
  override def toString(): String = "(" + left + "=" + right + ")"
  override def equals(o: Any): Boolean = {
    o match {
      case e : Equals => e.left.equals(left) && e.right.equals(right)
      case _ => false
    }
  }
}

