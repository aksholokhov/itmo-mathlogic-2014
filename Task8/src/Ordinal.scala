/**
 * Created by Шолохов on 21.04.2015.
 */
class Ordinal {
  sealed trait Ordinal

  case class +(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString(): String = l.toString + "+" + r.toString
  }

  case class *(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString(): String = l.toString + "*" + r.toString
  }

  case class ^(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString(): String = l.toString + "^" + r.toString
  }

  case class Num(v : Integer) extends Ordinal {
    override def toString(): String = v.toString
  }

  case class W() extends Ordinal {
    override def toString(): String = "w"
  }

  sealed trait CNF
}
