/**
 * Created by Шолохов on 02.02.2015.
 */
case class Plus(override val left: Expression, override val right: Expression) extends BinOp(left, right){
  override def apply(l : Boolean, r : Boolean) : Boolean = true
  override def toString(): String = "(" + left + "+" + right + ")"
  override def equals(o: Any): Boolean = {
    o match {
      case p : Plus => p.left.equals(left) && p.right.equals(right)
      case _ => false
    }
  }
}
