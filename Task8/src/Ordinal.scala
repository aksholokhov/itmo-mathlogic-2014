
  sealed trait Ordinal

  case class add(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString: String = "(" + l.toString + "+" + r.toString + ")"
  }

  case class mul(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString: String = "(" + l.toString + "*" + r.toString + ")"
  }

  case class pow(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString: String = "(" + l.toString + "^" + r.toString + ")"
  }

  case class Num(v : Integer) extends Ordinal {
    override def toString: String = v.toString
  }

  case class W() extends Ordinal {
    override def toString: String = "w"
  }





