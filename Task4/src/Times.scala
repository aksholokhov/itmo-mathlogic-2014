/**
 * Created by Шолохов on 02.02.2015.
 */
case class Times (override val left: Expression, override val right: Expression) extends BinOp(left, right){
  override def apply(l : Boolean, r : Boolean) : Boolean = true
  override def toString(): String = "(" + left + "*" + right + ")"
  override def equals(o: Any): Boolean = {
    o match {
      case t : Times => t.left.equals(left) && t.right.equals(right)
      case _ => false
    }
  }
}

