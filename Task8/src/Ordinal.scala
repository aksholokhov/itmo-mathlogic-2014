
  sealed trait Ordinal

  case class add(l: Ordinal, r: Ordinal) extends Ordinal {
    override def toString: String = "(" + l.toString + "+" + r.toString + ")"
  }

  case class mul(l: Ordinal, r: Ordinal) extends Ordinal {
    override def toString: String = "(" + l.toString + "*" + r.toString + ")"
  }

  case class pow(l: Ordinal, r: Ordinal) extends Ordinal {
    override def toString: String = "(" + l.toString + "^" + r.toString + ")"
  }

  case class Num(v: Int) extends Ordinal {
    override def toString: String = v.toString
  }

  case class W() extends Ordinal {
    override def toString: String = "w"
  }

  case class CList(list: List[(CNF, Int)], atom: Atom) extends CNF {
    override def toString = list.map(a => "(w" +
        (if (a._1 != Atom(1)) "^(" + a._1.toString + ")" else "")
        + ")"
        + (if (a._2 != 1) "*" + a._2.toString else ""))
        .mkString("+") + (if (atom != Atom(0)) "+" + atom.toString else "")
  }

  case class Atom(Int: Int) extends CNF {
    override def toString = Int.toString
  }

  object CNF {
    def addPair(pair: (CNF, Int), that: CNF) = that match {
      case t@Atom(n) => CList(List((pair._1, pair._2)), t)
      case CList(list, a) => CList((pair._1, pair._2) :: list, a)
    }

    def exp1(p: Atom, b: CNF): CNF = b match {
      case a if b.fe == Atom(1) => CList(List((Atom(b.fc), p.Int ^ b.rest.asInstanceOf[Atom].Int)), Atom(0))
      case a if b.rest.isInstanceOf[Atom] => CList(List((CList(List((b.fe - Atom(1), b.fc)), Atom(0)), p.Int ^ b.rest.asInstanceOf[Atom].Int)), Atom(0))
      case _ =>
        val c = exp1(p, b.rest)
        CList(List((addPair((b.fe - Atom(1), b.fc), c.fe),c.fc)), Atom(0))
    }

    def exp2(a: CNF, q: Atom): CNF = q match {
      case Atom(1) => a
      case Atom(n) => CList(List((a.fe * Atom(n - 1), 1)), Atom(0)) * a
    }

    def exp3(a: CNF, q: Atom): CNF = q match {
      case Atom(0) => Atom(1)
      case Atom(1) => a
      case x if a.limitp => exp2(a, q)
      case Atom(n) => exp3(a, Atom(n - 1)) * a
    }

    def exp4(a: CNF, b: CNF): CNF = CList(List((a.fe * b.limitpart, 1)), Atom(0)) * exp3(a, b.Intpart)
  }

  sealed trait CNF extends Ordered[CNF]{
    import CNF._
    override def compare(that: CNF): Int = this match
    {
      case Atom(n) => that match {
        case Atom(m) => n compare m
        case _ => -1
      }
      case CList(_, _) if (that match {
        case Atom(_) => true
        case _ => false
      }) => 1
      case CList(_, _) if this.fe != that.fe => this.fe compare that.fe
      case CList(_, _) if this.fc != that.fc => this.fc compare that.fc
      case _ => this.rest compare that.rest
    }

    def first: CNF = this match {
      case a: Atom => this
      case CList(list, a) => CList(list.head :: Nil, Atom(0))
    }

    def rest: CNF = this match {
      case CList(list, a) if list.length == 1 => a
      case CList(list, a) => CList(list.tail, a)
    }

    def firstn(n: Int): CNF = this.first match {
      case t : Atom => t
      case CList(l, _) => if (n > 1)
        this.rest.firstn(n - 1) match {
          case t : Atom => CList(l, t)
          case CList(l2, a) => CList(l ::: l2, a)
        }
      else
      if (n == 1) this.rest match {
        case t@Atom(_) => CList(l, t)
        case CList(l2, a) => CList(l ::: l2, Atom(0))
      } else throw new Exception("firstn")
    }

    def restn(n: Int): CNF = n match {
      case 0 => this
      case i => this.rest.restn(i - 1)
    }

    def fe: CNF = this match {
      case Atom(_) => Atom(0)
      case CList(list, _) => list.head._1
    }

    def fc: Int = this match {
      case Atom(n) => n
      case CList(list, _) => list.head._2
    }

    def length: Int = this match {
      case Atom(_) => 0
      case t@CList(_, _) => 1 + t.rest.length
    }

    def size: Int = this match {
      case Atom(_) => 1
      case t@CList(_, _) => t.fe.size + t.rest.size
    }

    def limitp: Boolean = this match {
      case Atom(n) => n == 0
      case t => t.rest.limitp
    }

    def limitpart: CNF = this match {
      case Atom(n) => Atom(0)
      case CList(_, _) => this.first match {
        case CList(l, _) => this.rest.limitpart match {
          case t@Atom(n) => CList(l, t)
          case CList(l2, a) => CList(l ::: l2, a)
        }
      }
    }

    def Intpart: Atom = this match {
      case t: Atom => t
      case _ => this.rest.Intpart
    }

    def +(that: CNF): CNF = this match {
      case Atom(n) if (that match {
        case Atom(m) => true
        case _ => false
      }) => Atom(n + that.asInstanceOf[Atom].Int)
      case _ =>
        if (this.fe < that.fe) that
        else if (this.fe == that.fe) addPair((this.fe, this.fc + that.fc), that.rest)
        else addPair((this.fe, this.fc), this.rest + that)
    }

    def -(that: CNF): CNF = this match {
      case Atom(n) if that.isInstanceOf[Atom] => that match {
        case Atom(m) => Atom(if (n <= m) 0 else n - m)
      }
      case a if this.fe < that.fe => Atom(0)
      case a if this.fe > that.fe => this
      case a if this.fc < that.fc => Atom(0)
      case a if this.fc > that.fc => addPair((this.fe, this.fc - that.fc), a.rest)
      case _ => this.rest - that.rest
    }

    def *(that: CNF): CNF = that match {
      case Atom(0) => Atom(0)
      case a if this == Atom(0) => Atom(0)
      case Atom(m) => this match {
        case Atom(n) => Atom(n * m)
        case _ => addPair((this.fe, this.fc * m), this.rest)
      }
      case _ => addPair((this.fe + that.fe, that.fc), this * that.rest)
    }


    def ^(that: CNF) = this match {
      case Atom(1) => Atom(1)
      case a if that == Atom(0) => Atom(1)
      case Atom(0) => Atom(0)
      case t@Atom(n) => that match {
        case Atom(m) => Atom(n ^ m)
        case _ => exp1(t, that)
      }
      case a if that.isInstanceOf[Atom] => exp3(this, that.asInstanceOf[Atom])
      case _ => exp4(this, that)
    }
  }


