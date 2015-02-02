/**
 * Created by Шолохов on 16.11.2014.
 */
case class Conjunction ( left : Expression, right : Expression) extends BinOp (left, right){
  override def apply(l: Boolean, r: Boolean): Boolean = l && r

  override def equals(o: Any): Boolean = {
    if (o.isInstanceOf[Conjunction]) {
      val n = o.asInstanceOf[Conjunction]
      left.equals(n.left) && right.equals(n.right)
    } else false
  }

  override def toString() : String = {
    return  "(" + left.toString + "&" + right.toString + ")"
  }
}
