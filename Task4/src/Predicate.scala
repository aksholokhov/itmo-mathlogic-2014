import scala.collection.mutable.ArrayBuffer

/**
 * Created by Шолохов on 30.01.2015.
 */
case class Predicate(name : String, terms : ArrayBuffer[Expression]) extends Expression {
  override def evaluate(values: Map[String, Boolean]): Boolean = true
  override def toString(): String = {
    var s : String = name
    if (terms != null && terms.size != 0) {
      s += "("
    }
    if (terms != null && terms.size != 0) {
      terms.foreach(t => s += t + ",")
      //s = s.dropRight(1)
    }
    if (terms != null && terms.size != 0) {
      s += ")"
    }
    s
  }

  override def equals(o : Any): Boolean = {
    o match {
      case t : Predicate =>
        var res = name.equals(t.name) && terms.size == t.terms.size
        if (res) {
          terms.zip(t.terms).foreach(x => res = res && x._1.equals(x._2))
        }
        res
      case _ => false
    }
  }
}
