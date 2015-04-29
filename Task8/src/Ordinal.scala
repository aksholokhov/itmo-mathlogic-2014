
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

  case class Atom(n: Int) extends CNF {
    override def toString: String = "(" + n.toString + ")"
  }


  case class CList(list: List[(CNF, Int)], atom: Atom) extends CNF {
    if (list.size == 0) throw new Exception("nil in CList")
  /*  override def toString: String =
      list.map(p => "(w " + (if (p._1 == Atom(1)) ""
      else "^(" + p._1.toString + ")"
        + (if (p._2 == 1) "" else "*" + p._2.toString)
        + ")"))
        .mkString("+") + (if (atom == Atom(0)) "" else "+" + atom.toString) */

  }




  sealed trait CNF extends Ordered[CNF] {

    def ordToCantor(o: Ordinal): CNF = o match {
      case W() => CList(List((Atom(1), 1)), Atom(0))
      case Num(a) => Atom(a)
      case add(a, b) => ordToCantor(a) + ordToCantor(b)
      case mul(a, b) => ordToCantor(a) * ordToCantor(b)
      case pow(a, b) => ordToCantor(a) ^ ordToCantor(b)
    }

    def compare(that: CNF): Int = this match {
      case Atom(n) => that match {
        case Atom(m) => n.compareTo(m)
        case _ => -1
      }
      case CList(_, _) if (that match {
        case Atom(_) => true
        case _ => false
      }) => 1
      case CList(_, _) if fe(this) != fe(that) => fe(this).compare(fe(that))
      case CList(_, _) if fc(this) != fc(that) => fc(this).compare(fc(that))
      case _ => rest(this).compare(rest(that))
    }


    def +(b: CNF): CNF = this match {
      case Atom(n) if (b match {
        case Atom(m) => true
        case _ => false
      }) => Atom(n + b.asInstanceOf[Atom].n)
      case _ => if (fe(this) < fe(b)) b
      else if (fe(this) == fe(b)) addPair((fe(this), fc(this) + fc(b)), rest(b))
      else addPair((fe(this), fc(b)), rest(this) + b)

    }

    def -(b: CNF): CNF = this match {
      case Atom(n) => b match {
        case Atom(m) => if (n <= m) Atom(0) else Atom(n - m)
        case _ => throw new Exception("Atom - NonAtom: " + this + "-" + b)
      }
      case a if fe(a) < fe(b) => Atom(0)
      case a if fe(a) > fe(b) => a
      case a if fc(a) < fc(b) => Atom(0)
      case a if fc(a) > fc(b) => addPair((fe(a), fc(a) - fc(b)), rest(a))
      case _ => rest(this) - rest(b)
    }

    def *(b: CNF): CNF = b match {
      case Atom(0) => Atom(0)
      case _ if this == Atom(0) => Atom(0)
      case Atom(m) => this match {
        case Atom(n) => Atom(n * m)
        case a => addPair((fe(a), fc(a) * m), rest(a))
      }
      case a => addPair((fe(a) + fe(b), fc(b)), a * rest(b))
    }

    def ^(b: CNF) = this match {
      case Atom(1) => Atom(1)
      case _ if b == Atom(0) => Atom(1)
      case t@Atom(n) => b match {
        case Atom(m) => Atom(n ^ m)
        case _ => exp1(t, b)
      }
      case _ if b.isInstanceOf[Atom] => exp3(this, b.asInstanceOf[Atom])
      case _ => exp4(this, b)
    }


    def first(a: CNF): CNF = a match {
      case e: Atom => e
      case CList(list, p) => CList(list.head :: Nil, Atom(0))
    }

    def rest(a: CNF): CNF = a match {
      case e: Atom => throw new Exception("atom in rest")
      case CList(list, p) if list.length == 1 => p //WHY?
      case CList(list, p) => CList(list.tail, p)
    }

    def firstn(a: CNF, n: Int): CNF = a match {
      case t: Atom => t
      case CList(list, _) => n match {
        case _ if n > 1 => rest(firstn(a, n - 1)) match {
          case t: Atom => CList(list, t)
          case CList(list2, t) => CList(list ::: list2, t)
        }
        case 1 => rest(a) match {
          case t: Atom => CList(list, t)
          case CList(list2, _) => CList(list ::: list2, Atom(0))
        }
        case _ => throw new Exception("n = 0 in firstn")
      }
    }

    def restn(a: CNF, n: Int): CNF = n match {
      case 0 => a
      case _ => rest(restn(a, n - 1))
    }

    def fe(a: CNF): CNF = a match {
      case Atom(_) => Atom(0)
      case CList(list, _) => list.head._1
    }

    def fc(a: CNF): Int = a match {
      case Atom(n) => n
      case CList(list, _) => list.head._2
    }

    def length(a: CNF): Int = a match {
      case Atom(_) => 0
      case CList(_, _) => length(rest(a)) + 1
    }

    def size(a: CNF): Int = a match {
      case Atom(_) => 1
      case p@CList(_, _) => size(fe(p)) + size(rest(p))
    }

    def limitp(a: CNF): Boolean = a match {
      case Atom(n) => n == 0
      case t: CList => limitp(rest(t))
    }

    def limitpart(a: CNF): CNF = a match {
      case Atom(n) => Atom(0)
      case CList(_, _) => first(this) match {
        case CList(list, _) => limitpart(rest(this)) match {
          case n: Atom => CList(list, n)
          case CList(list2, n) => CList(list ::: list2, n)
        }
      }
      case _ => throw new Exception("Pattern matching error in limitpart")
    }

    def natpart(a: CNF): Atom = a match {
      case t: Atom => t
      case _ => natpart(rest(a))
    }

    def addPair(pair: (CNF, Int), b: CNF): CNF = b match {
      case t: Atom => CList(List(pair), t)
      case CList(list, t) => CList(pair :: list, t)
    }

    def exp1(t: Atom, b: CNF): CNF = b match {
      case a if fe(b) == Atom(1) => CList(
        List((Atom(fc(b)), t.n ^ rest(b).asInstanceOf[Atom].n)),
        Atom(0)
      )
      case a if rest(b).isInstanceOf[Atom] => CList(
        List((
          CList(
            List((fe(b) - Atom(1), fc(b))),
            Atom(0)),
          t.n ^ rest(b).asInstanceOf[Atom].n)),
        Atom(0))
      case _ =>
        val p: CNF = exp1(t, rest(b))
        CList(
          List((addPair((fe(b) - Atom(1), fc(b)), fe(p)), fc(p))),
          Atom(0)
        )
    }

    def exp2(a: CNF, q: Atom): CNF = q match {
      case Atom(1) => a
      case Atom(n) => CList(List((fe(a) * Atom(n - 1), 1)), Atom(0)) * a
    }

    def exp3(a: CNF, q: Atom): CNF = q match {
      case Atom(0) => Atom(1)
      case Atom(1) => a
      case x if limitp(a) => exp2(a, q)
      case Atom(n) => exp3(a, Atom(n - 1)) * a
    }

    def exp4(a: CNF, b: CNF): CNF = CList(List((fe(a) * limitpart(b), 1)), Atom(0)) * exp3(a, natpart(b))
  }





