
  sealed trait Ordinal

  case class equ(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString: String = l.toString + "=" + r.toString
  }

  case class add(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString: String = l.toString + "+" + r.toString
  }

  case class mul(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString: String = l.toString + "*" + r.toString
  }

  case class pow(l: Ordinal, r: Ordinal) extends Ordinal{
    override def toString: String = l.toString + "^" + r.toString
  }

  case class Num(v : Integer) extends Ordinal {
    override def toString: String = v.toString
  }

  case class W() extends Ordinal {
    override def toString: String = "w"
  }


  sealed trait CNF {
    case class Atom(n : Integer) extends CNF {
      override def toString: String = n.toString
    }

    val zero = new Atom(0)
    val one = new Atom(1)

    case class CList(list : List[(CNF, Integer)], atom : Atom) extends CNF {
      if (list.size == 0) throw new Exception("nil in CList")
      override def toString: String =
        list.map(p => "(w "
          + (if (p._1 == one) "" else "^(" + p._1.toString + ")"
          + (if (p._2 == 1) "" else "*" + p._2.toString)
          +  ")"))
          .mkString("+") + (if (atom == zero) "" else "+" + atom.toString)

    }
  }





