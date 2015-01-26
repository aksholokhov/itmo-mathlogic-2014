/**
 * Created by Шолохов on 16.11.2014.
 */
case class Disjunction ( val left : Expression, val right : Expression) extends BinOp (left, right){
  override def apply(l: Boolean, r: Boolean): Boolean = l || r

  override def equals(o: Any): Boolean = {
    if (o.isInstanceOf[Disjunction]) {
      val n = o.asInstanceOf[Disjunction]
      left.equals(n.left) && right.equals(n.right)
    } else false
  }

  override def toString() : String = {
    return  "(" + left.toString + "|" + right.toString + ")"
  }
}
